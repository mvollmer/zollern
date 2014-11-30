#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <SDL.h>

__attribute__ ((noreturn))
void
exitf (int code, char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  exit (code);
}

#define WIDTH  512
#define HEIGHT 512
#define PIXELS_SIZE (sizeof(unsigned int) * WIDTH * HEIGHT)

int pixels_fd;
unsigned int *pixels;

void
init_pixels ()
{
  pixels_fd = shm_open ("/zollern-fb", O_CREAT | O_RDWR, 0666);
  if (pixels_fd < 0)
    exitf (1, "pixels: %m");

  if (ftruncate (pixels_fd, PIXELS_SIZE) < 0)
    exitf (1, "pixels: %m");

  pixels = mmap (NULL, PIXELS_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
                 pixels_fd, 0);
  if (pixels == MAP_FAILED)
    exitf (1, "pixels: %m");
}

SDL_Window *window;
SDL_Surface *surface;
SDL_Surface *fb;

void
paint ()
{
  surface = SDL_GetWindowSurface (window);
  SDL_FillRect (surface, NULL, SDL_MapRGB(surface->format, 0x80, 0x80, 0xFF));
  SDL_BlitSurface (fb, NULL, surface, NULL);
  SDL_UpdateWindowSurface (window);
}

void
main ()
{
  init_pixels ();

  if (SDL_Init (SDL_INIT_VIDEO) < 0)
    {
      printf("%s\n", SDL_GetError());
      return;
    }

  window = SDL_CreateWindow ("SDL",
                             SDL_WINDOWPOS_UNDEFINED,
                             SDL_WINDOWPOS_UNDEFINED,
                             WIDTH,
                             HEIGHT,
                             SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);

  if (window == NULL)
    {
      printf("%s\n", SDL_GetError());
      return;
    }

  fb = SDL_CreateRGBSurfaceFrom (pixels, WIDTH, HEIGHT,
                                 32, 4*WIDTH,
                                 0x00FF0000,
                                 0x0000FF00,
                                 0x000000FF,
                                 0x00000000);
  if (fb == NULL)
    {
      printf("%s\n", SDL_GetError());
      return;
    }

  int i;
  for (i = 0; i < 256; i++)
    pixels[i*WIDTH+i] = 0x00FFFF00;

  paint ();

  SDL_Event e;
  while (SDL_WaitEvent (&e) != 0)
    {
      if (e.type == SDL_QUIT)
        exit (0);
      else if (e.type == SDL_WINDOWEVENT)
        {
          if (e.window.event == SDL_WINDOWEVENT_RESIZED)
            paint ();
        }
    }

  printf("%s\n", SDL_GetError());
}
