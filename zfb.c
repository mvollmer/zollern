#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <poll.h>

#include <SDL.h>
#include <SDL_syswm.h>

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

int verbose;

uint32_t *pixels;

#define PIXELS_FD   3
#define COMMANDS_FD 4
#define EVENTS_FD   5

int commands_in_fd;
int events_out_fd;

void
init_pixels ()
{
  int fd = shm_open ("/zollern-fb", O_CREAT | O_RDWR, 0600);
  if (fd < 0)
    exitf (1, "pixels: %m");

  if (fd != PIXELS_FD)
    {
      if (dup2 (fd, PIXELS_FD) < 0)
        exitf (1, "pixels dup: %m");
      close (fd);
    }

  if (fcntl (PIXELS_FD, F_SETFD, 0) < 0)
    exitf (1, "pixels fcntl: %m");

  shm_unlink ("/zollern-fb");
}

void
shuffle_pipe (int *for_us, int *for_child, int child_fd)
{
  if (*for_us == child_fd)
    {
      int fd = dup (*for_us);
      if (fd < 0)
        exitf (1, "pipe dup: %m");
      close (*for_us);
      *for_us = fd;
    }

  if (*for_child != child_fd)
    {
      if (dup2 (*for_child, child_fd) < 0)
        exitf (1, "pipe dup: %m");
      close (*for_child);
      *for_child = child_fd;
    }
}

void
init_pipes ()
{
  int commands[2];
  int events[2];

  if (pipe (commands) < 0)
    exitf (1, "command pipe: %m");

  shuffle_pipe (&commands[0], &commands[1], COMMANDS_FD);
  commands_in_fd = commands[0];

  if (pipe (events) < 0)
    exitf (1, "events pipe: %m");

  shuffle_pipe (&events[1], &events[0], EVENTS_FD);
  events_out_fd = events[1];
}


struct command {
  int32_t op;
  int32_t arg1;
  int32_t arg2;
  int32_t arg3;
};

enum {
  OP_CONF = 1,
  OP_SHOW = 2
};

struct event {
  int16_t type;
  int16_t x;
  int16_t y;
  int16_t state;
  int32_t input;
};

enum {
  EV_QUIT = 1,
  EV_INPUT = 2,
  EV_SIZE = 3
};

enum {
  EV_MOVE = 0,
  EV_BTN_1_PRESS,
  EV_BTN_1_RELEASE,
  EV_BTN_2_PRESS,
  EV_BTN_2_RELEASE,
  EV_BTN_3_PRESS,
  EV_BTN_3_RELEASE,
  EV_SCR_UP,
  EV_SCR_DOWN,
  EV_KEY_BACKSPACE,
  EV_KEY_TAB,
  EV_KEY_RETURN,
  EV_KEY_ESCAPE,
  EV_KEY_DELETE,
  EV_KEY_HOME,
  EV_KEY_LEFT,
  EV_KEY_UP,
  EV_KEY_RIGHT,
  EV_KEY_DOWN,
  EV_KEY_PAGE_UP,
  EV_KEY_PAGE_DOWN,
  EV_KEY_END,
  EV_KEY_BEGIN
};

enum {
  EV_STATE_BTN_1   =  1,
  EV_STATE_BTN_2   =  2,
  EV_STATE_BTN_3   =  4,
  EV_STATE_SHIFT   =  8,
  EV_STATE_CONTROL = 16,
  EV_STATE_META    = 32
};

void
send_event (int type, int x, int y, int state, int input)
{
  struct event ev = { type, x, y, state, input };
  int n = write (events_out_fd, &ev, sizeof (ev));
  if (n < 0)
    exitf (1, "event: %m");
  else if (n != sizeof (ev))
    exitf (1, "event: short write");
}

SDL_Window *window;
SDL_Surface *fb;

void
setup_window ()
{
  window = SDL_CreateWindow ("Z",
                             SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 512, 512,
                             SDL_WINDOW_MAXIMIZED | SDL_WINDOW_BORDERLESS | SDL_WINDOW_RESIZABLE);

  if (window == NULL)
    exitf (1, "%s\n", SDL_GetError());
}

void
configure (int width, int height)
{
  if (fb)
    {
      int ow = fb->w, oh = fb->h;
      SDL_FreeSurface (fb);
      munmap (pixels, ow*oh*4);
    }

  pixels = mmap (NULL, width*height*4, PROT_READ | PROT_WRITE, MAP_SHARED,
                 PIXELS_FD, 0);

  if (pixels == MAP_FAILED)
    exitf (1, "pixels map: %m");

  fb = SDL_CreateRGBSurfaceFrom (pixels, width, height,
                                 32, 4*width,
                                 0x00FF0000,
                                 0x0000FF00,
                                 0x000000FF,
                                 0x00000000);
  if (fb == NULL)
    exitf (1, "%s\n", SDL_GetError());
}

void
show ()
{
  SDL_Surface *surface = SDL_GetWindowSurface (window);
  SDL_BlitSurface (fb, NULL, surface, NULL);
  SDL_UpdateWindowSurface (window);
}

int
input_from_code (SDL_Keycode sym, SDL_Keymod mod)
{
  switch (sym)
    {
    case SDLK_BACKSPACE:
      return -EV_KEY_BACKSPACE;
    case SDLK_TAB:
      return -EV_KEY_TAB;
    case SDLK_RETURN:
      return -EV_KEY_RETURN;
    case SDLK_ESCAPE:
      return -EV_KEY_ESCAPE;
    case SDLK_DELETE:
      return -EV_KEY_DELETE;
    case SDLK_LEFT:
      return -EV_KEY_LEFT;
    case SDLK_UP:
      return -EV_KEY_UP;
    case SDLK_RIGHT:
      return -EV_KEY_RIGHT;
    case SDLK_DOWN:
      return -EV_KEY_DOWN;
    case SDLK_PAGEUP:
      return -EV_KEY_PAGE_UP;
    case SDLK_PAGEDOWN:
      return -EV_KEY_PAGE_DOWN;
    case SDLK_END:
      return -EV_KEY_END;
    case SDLK_HOME:
      return -EV_KEY_BEGIN;
    default:
      if ((mod & ~KMOD_SHIFT) != 0 && sym < 128)
        return sym;
      else
        return 0;
    }
}

int
state_from_mod (SDL_Keymod mod)
{
  int state = 0;
  if (mod & KMOD_SHIFT)
    state |= EV_STATE_SHIFT;
  if (mod & KMOD_CTRL)
    state |= EV_STATE_CONTROL;
  if (mod & KMOD_ALT)
    state |= EV_STATE_META;
  return state;
}

int command_event_type;

void
execute_command (struct command *cmd)
{
  if (verbose)
    printf ("C %d\n", cmd->op);

  switch (cmd->op)
    {
    case OP_CONF:
      configure (cmd->arg1, cmd->arg2);
      break;
    case OP_SHOW:
      show ();
      break;
    }
}

void
handle_event (SDL_Event *e)
{
  if (e->type == SDL_QUIT)
    {
      send_event (EV_QUIT, 0, 0, 0, 0);
    }
  else if (e->type == SDL_WINDOWEVENT
           && e->window.event == SDL_WINDOWEVENT_SIZE_CHANGED)
    {
      send_event (EV_SIZE, e->window.data1, e->window.data2, 0, 0);
    }
  else if (e->type == SDL_TEXTINPUT)
    {
      int mod = SDL_GetModState();
      if ((mod & ~KMOD_SHIFT) == 0)
        {
          // XXX - include mouse position and button state
          send_event (EV_INPUT, 0, 0, 0, e->text.text[0]);
        }
    }
  else if (e->type == SDL_KEYDOWN)
    {
      int input = input_from_code (e->key.keysym.sym, e->key.keysym.mod);
      if (input != 0)
        {
          int state = state_from_mod (e->key.keysym.mod);
          if (input > 0)
            state &= ~EV_STATE_SHIFT;
          // XXX - include mouse position and button state
          send_event (EV_INPUT, 0, 0, state, input);
        }
    }
  else if (e->type == command_event_type)
    {
      struct command cmd;
      cmd.op = e->wheel.which;
      cmd.arg1 = e->wheel.x;
      cmd.arg2 = e->wheel.y;
      cmd.arg3 = e->wheel.direction;
      execute_command (&cmd);
    }

  // XXX - mouse events
}

int
read_commands (void *unused)
{
  struct command cmd;

  while (1)
    {
      int n = read (commands_in_fd, &cmd, sizeof (cmd));
      if (n == 0)
        exitf (0, "done");
      else if (n < 0)
        exitf (1, "read: %m");
      else if (n != sizeof (cmd))
        exitf (1, "short read: %d", n);

      SDL_Event event;
      event.type = command_event_type;
      event.wheel.which = cmd.op;
      event.wheel.x = cmd.arg1;
      event.wheel.y = cmd.arg2;
      event.wheel.direction = cmd.arg3;
      SDL_PushEvent (&event);
    }
}

void
usage ()
{
  exitf (1, "usage: fb [-g] CMD...");
}

void
main (int argc, char **argv)
{
  int debug = 0;
  int i;
  const char *debug_argv[20];
  const char *prog;
  char **args;

  init_pixels ();
  init_pipes ();

  if (SDL_Init (SDL_INIT_VIDEO) < 0)
    {
      printf("%s\n", SDL_GetError());
      return;
    }

  argv++;
  while (argv[0] && argv[0][0] == '-')
    {
      if (strcmp (argv[0], "-g") == 0)
        debug = 1;
      else if (strcmp (argv[0], "-v") == 0)
        verbose = 1;
      else
        usage ();
      argv++;
    }

  if (!argv[0])
    usage ();

  if (debug)
    {
      i = 0;
      debug_argv[i++] = "gdb";
      debug_argv[i++] = "--args";
      while (argv[0])
        debug_argv[i++] = *argv++;
      debug_argv[i++] = NULL;
      prog = "gdb";
      args = (char **)debug_argv;
    }
  else
    {
      prog = argv[0];
      args = (char **)argv;
    }

  int pid = fork ();
  if (pid < 0)
    exitf (1, "fork: %m");

  if (pid == 0)
    {
      close (events_out_fd);
      close (commands_in_fd);
      execvp (prog, args);
      write (2, "exec\n", 5);
      _exit (0);
    }

  close (EVENTS_FD);
  close (COMMANDS_FD);

  setup_window ();

  command_event_type = SDL_RegisterEvents (1);
  SDL_CreateThread (read_commands, "command pump", NULL);

  while (1)
    {
      SDL_Event event;
      if (!SDL_WaitEvent (&event))
        exitf (1, "Error while reading SDL events: %s\n", SDL_GetError());
      handle_event (&event);
    }
}
