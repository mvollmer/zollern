#include <stdbool.h>

#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/wait.h>
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
  OP_SHOW_AND_GET = 2,
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
SDL_Renderer *renderer;
SDL_Texture *texture;
int tx_width, tx_height;

float scale = 1.0;

void
setup_window ()
{
  if (SDL_GetNumVideoDisplays() == 1)
    scale = 1.25;

  window = SDL_CreateWindow ("Z",
                             SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                             1280*scale, 1024*scale,
                             SDL_WINDOW_SHOWN);

  if (window == NULL)
    exitf (1, "%s\n", SDL_GetError());

  renderer = SDL_CreateRenderer (window, -1, 0);
}

void
configure (int width, int height)
{
  if (width == tx_width && height == tx_height)
    return;

  if (texture)
    {
      SDL_DestroyTexture (texture);
      munmap (pixels, tx_width*tx_height*4);
    }

  pixels = mmap (NULL, width*height*4, PROT_READ | PROT_WRITE, MAP_SHARED,
                 PIXELS_FD, 0);

  if (pixels == MAP_FAILED)
    exitf (1, "pixels map: %m");

  tx_width = width;
  tx_height = height;
  texture = SDL_CreateTexture (renderer,
                               SDL_PIXELFORMAT_ARGB8888,
                               SDL_TEXTUREACCESS_STREAMING,
                               width, height);
  if (texture == NULL)
    exitf (1, "%s\n", SDL_GetError());
  if (scale != 1.0)
    SDL_SetTextureScaleMode (texture, SDL_ScaleModeBest);
}

void
show ()
{
  void *tx_pixels;
  int tx_pitch;

  SDL_LockTexture (texture, NULL, &tx_pixels, &tx_pitch);
  memcpy (tx_pixels, pixels, tx_width * tx_height * 4);
  SDL_UnlockTexture (texture);
  SDL_RenderCopy (renderer, texture, NULL, NULL);
  SDL_RenderPresent (renderer);
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
      if ((mod & (KMOD_CTRL|KMOD_ALT)) && sym < 128)
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

int
state_from_buttons (int buttons)
{
  int state = 0;
  if (buttons & SDL_BUTTON(SDL_BUTTON_LEFT))
    state |= EV_STATE_BTN_1;
  if (buttons & SDL_BUTTON(SDL_BUTTON_MIDDLE))
    state |= EV_STATE_BTN_2;
  if (buttons & SDL_BUTTON(SDL_BUTTON_RIGHT))
    state |= EV_STATE_BTN_3;
  return state;
}

void
send_input_event (int input, int mod)
{
  int x, y, state;
  state = state_from_mod (mod) | state_from_buttons (SDL_GetMouseState(&x, &y));
  x /= scale;
  y /= scale;
  if (input > 0)
    state &= ~EV_STATE_SHIFT;
  send_event (EV_INPUT, x, y, state, input);
}

bool
handle_event (SDL_Event *e)
{
  if (e->type == SDL_QUIT)
    {
      send_event (EV_QUIT, 0, 0, 0, 0);
      return true;
    }
  else if (e->type == SDL_WINDOWEVENT
           && e->window.event == SDL_WINDOWEVENT_SIZE_CHANGED)
    {
      // send_event (EV_SIZE, e->window.data1, e->window.data2, 0, 0);
    }
  else if (e->type == SDL_TEXTINPUT)
    {
      // XXX - SDL reports KMOD_RALT for AltGr, so let's hack the three
      //       cases on my keyboard that I use AltGr for...
      int mod = SDL_GetModState();
      if (e->text.text[0] == '~' || e->text.text[0] == '@' || e->text.text[0] == '|')
        mod &= ~KMOD_RALT;
      send_input_event (e->text.text[0], mod);
      return true;
    }
  else if (e->type == SDL_KEYDOWN)
    {
      int input = input_from_code (e->key.keysym.sym, e->key.keysym.mod);
      if (input != 0) {
          send_input_event (input, e->key.keysym.mod);
          return true;
      }
    }
  else if (e->type == SDL_MOUSEBUTTONDOWN)
    {
      int input = 0;
      if (e->button.button == SDL_BUTTON_LEFT)
        input = -EV_BTN_1_PRESS;
      else if (e->button.button == SDL_BUTTON_MIDDLE)
        input = -EV_BTN_2_PRESS;
      else if (e->button.button == SDL_BUTTON_RIGHT)
        input = -EV_BTN_3_PRESS;
      if (input) {
        send_input_event (input, SDL_GetModState());
        return true;
      }
    }
  else if (e->type == SDL_MOUSEBUTTONUP)
    {
      int input = 0;
      if (e->button.button == SDL_BUTTON_LEFT)
        input = -EV_BTN_1_RELEASE;
      else if (e->button.button == SDL_BUTTON_MIDDLE)
        input = -EV_BTN_2_RELEASE;
      else if (e->button.button == SDL_BUTTON_RIGHT)
        input = -EV_BTN_3_RELEASE;
      if (input) {
        send_input_event (input, SDL_GetModState());
        return true;
      }
    }
  else if (e->type == SDL_MOUSEWHEEL)
    {
      int input = 0;
      if (e->wheel.y > 0)
        input = -EV_SCR_UP;
      else if (e->wheel.y < 0)
        input = -EV_SCR_DOWN;
      if (input) {
        send_input_event (input, SDL_GetModState());
        return true;
      }
    }
  else if (e->type == SDL_MOUSEMOTION)
    {
      if (e->motion.state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
        send_input_event (-EV_MOVE, SDL_GetModState());
        return true;
      }
    }

  return false;
}

int
read_command (struct command *cmd)
{
  int n = read (commands_in_fd, cmd, sizeof (*cmd));
  if (n == 0)
    return 0;
  else if (n < 0)
    exitf (1, "read: %m");
  else if (n != sizeof (*cmd))
    exitf (1, "short read: %d", n);
  else
    return 1;
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

  setenv ("SDL_VIDEODRIVER", "wayland", 0);

  init_pixels ();

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

  setup_window ();

 again:
  init_pipes ();

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

  send_event(EV_SIZE, 1280, 1024, 0, 0);

  while (1)
    {
      struct command cmd;
      SDL_Event event;
      int n, status;

      if (!read_command (&cmd)) {
        close (events_out_fd);
        close (commands_in_fd);
        waitpid (pid, &status, 0);
        if (WIFEXITED(status) && WEXITSTATUS(status) == 77)
          goto again;
        return;
      }

      if (cmd.op == OP_CONF) {
        configure (cmd.arg1, cmd.arg2);
      } else if (cmd.op == OP_SHOW_AND_GET) {
        // Eat pending key repeat events to slow things down a bit.
        // Otherwise my six year old laptop can't keep up, especially
        // when it runs on battery...
        SDL_PumpEvents ();
        while (true) {
          n = SDL_PeepEvents (&event, 1,
                              SDL_PEEKEVENT, SDL_KEYDOWN, SDL_KEYDOWN);
          if (n > 0 &&
              ((event.type == SDL_KEYDOWN && event.key.repeat)
               || (event.type == SDL_MOUSEMOTION))) {
            SDL_PeepEvents (&event, 1,
                            SDL_GETEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT);
          } else
            break;
        }

        show ();

        do {
          if (!SDL_WaitEvent (&event))
            exitf (1, "Error while reading SDL events: %s\n", SDL_GetError());
        } while (!handle_event (&event));
      } else {
        exitf (1, "Unknown command: %d\n", cmd.op);
      }
    }
}
