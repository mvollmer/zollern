/* zfs - build a disk image for Zollern
 *
 * Version 0 is more than trivial and hasn't any features to cope with
 * interruptions in the middle of writing, or with media failures.
 *
 * Version 0 layout
 *
 * | toc | data_1 | ... | data_N |
 *
 * Both toc and data blocks have fixed sizes of 16 KiB.
 *
 * The toc block stores names for the files and pointers to their data
 * blocks:
 *
 * | header | meta_1 | ... | meta_M |
 *
 * A meta slot is 256 bytes and looks like this:
 *
 * | name | size | ptr_1 | ... | ptr_L |
 *
 * The name is 64 bytes, size is 4 bytes and each ptr is 2 bytes.
 * Thus,
 *
 *    M = 16 * 1024 / 256 - 1 = 63,
 *    L = (256 - 64 - 4) / 2 = 94,
 *    N = L * M = 5922.
 *
 * The header is also 256 bytes and looks like this:
 *
 * | magic | version | unused |
 *
 */

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>

#define block_size (16*1024)
#define slot_size  256
#define name_size  64

#define n_slots    (block_size / slot_size - 1)
#define n_ptrs     ((slot_size - name_size - sizeof(uint32_t)) / sizeof(uint16_t))
#define n_data     (n_slots * n_ptrs)

struct header {
  uint8_t magic[4];
  uint32_t version;
  uint8_t padding[slot_size - 2*sizeof(uint32_t)];
};

struct meta {
  uint8_t name[name_size];
  uint32_t size;
  uint16_t ptr[n_ptrs];
};

struct toc {
  struct header header;
  struct meta meta[n_slots];
};

struct data {
  uint8_t bytes[block_size];
};

struct disk {
  struct toc toc;
  struct data data[n_data];
};

void
create (char *dir, char *file)
{
  char fn[256];
  int fd;

  struct disk *disk = malloc(sizeof(struct disk));
  memset(disk, 0, sizeof(struct disk));

  strncpy (disk->toc.header.magic, "ZOLL", 4);
  disk->toc.header.version = 0;

  int next_slot = 0;
  int next_data = 0;

  DIR *dd = opendir (dir);
  struct dirent *de;
  while (de = readdir (dd))
    {
      if (strcmp (de->d_name, ".") == 0
          || strcmp (de->d_name, "..") == 0)
        continue;

      int s = next_slot++;
      strncpy (disk->toc.meta[s].name, de->d_name, name_size);
      int sz = 0;
      strcpy (fn, dir);
      strcat (fn, "/");
      strcat (fn, de->d_name);
      fd = open (fn, O_RDONLY);
      for (int i = 0; i < n_ptrs; i++)
        {
          int n = read (fd, disk->data[next_data].bytes, block_size);
          if (n == 0)
            break;
          sz += n;
          disk->toc.meta[s].ptr[i] = ++next_data;
          next_data += 2;  // force discontinuity to make it more interesting
          if (n < block_size)
            break;
        }
      close (fd);
      disk->toc.meta[s].size = sz;
    }
  closedir (dd);

  fd = open (file, O_WRONLY | O_CREAT, 0666);
  write (fd, disk, sizeof(struct disk));
  close (fd);
}

void
extract (char *file, char *dir)
{
  char fn[256];
  int fd;

  struct disk *disk = malloc(sizeof(struct disk));

  fd = open (file, O_RDONLY);
  read (fd, disk, sizeof(struct disk));
  close (fd);

  for (int s = 0; s < n_slots; s++)
    {
      if (*disk->toc.meta[s].name)
        {
          strcpy (fn, dir);
          strcat (fn, "/");
          strcat (fn, disk->toc.meta[s].name);
          fd = open(fn, O_WRONLY | O_CREAT, 0666);

          int sz = disk->toc.meta[s].size;
          int i = 0;
          while (sz > 0)
            {
              int n = block_size;
              if (sz < n)
                n = sz;
              write (fd, disk->data[disk->toc.meta[s].ptr[i]-1].bytes, n);
              sz -= n;
              i += 1;
            }
          close(fd);
        }
    }
}

void usage()
{
  fprintf (stderr, "usage: zfs create DIR DISK\n");
  fprintf (stderr, "       zfs extract DISK\n");
  exit (1);
}

void
main (int argc, char **argv)
{
  if (strcmp (argv[1], "create") == 0)
    create (argv[2], argv[3]);
  else if (strcmp (argv[1], "extract") == 0)
    extract (argv[2], argv[3]);
  else
    usage ();
}
