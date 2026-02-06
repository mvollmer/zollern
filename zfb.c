/* pixel.c
 *
 * Compile: cc -ggdb pixel.c -o pixel $(pkg-config --cflags --libs gtk4) -o pixel
 * Run: ./pixel
 */

#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <poll.h>

#include <gtk/gtk.h>

#define WIDTH  1280
#define HEIGHT 1024

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

guint32 *pixels;
guint32 *stable_pixels;

cairo_surface_t *pixel_surface;
int sf_width, sf_height;
GtkWidget *window;
GtkWidget *drawing_area;

#define PIXELS_FD   3
#define COMMANDS_FD 4
#define EVENTS_FD   5

int commands_read_fd;
int commands_write_fd;
int events_read_fd;
int events_write_fd;

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

#define MAX_EVENT_QUEUE 10

struct event event_queue[MAX_EVENT_QUEUE];
int n_events_queued;
int n_events_wanted;

void
process_event_queue ()
{
  // printf ("processing %d %d\n", n_events_queued, n_events_wanted);
  while (n_events_queued > 0 && n_events_wanted > 0) {
    struct event *ev = &event_queue[0];
    // printf ("sending %d %d %d\n", events_write_fd, ev->type, ev->input);
    int n = write (events_write_fd, ev, sizeof (*ev));
    if (n < 0)
      exitf (1, "event: %m");
    else if (n != sizeof (*ev))
      exitf (1, "event: short write");

    n_events_wanted -= 1;
    n_events_queued -= 1;
    memcpy (event_queue, event_queue + 1, n_events_queued * sizeof(struct event));
  }
}

void
queue_event (int type, int x, int y, int state, int input)
{
  if (n_events_queued == MAX_EVENT_QUEUE)
    {
      printf("too many queued events, dropping\n");
      return;
    }

  struct event *ev = &event_queue[n_events_queued];
  ev->type = type;
  ev->x = x;
  ev->y = y;
  ev->state = state;
  ev->input = input;

  n_events_queued += 1;
  process_event_queue();
}

void
want_event()
{
  n_events_wanted += 1;
  process_event_queue();
}

static void draw_func(GtkDrawingArea *area,
                      cairo_t *cr,
                      int width,
                      int height,
                      gpointer user_data)
{
  if (pixel_surface)
    {
      cairo_set_source_surface(cr, pixel_surface, 0, 0);
      cairo_rectangle(cr, 0, 0, width, height);
      cairo_fill(cr);
    }
}

void
configure (int width, int height)
{
  if (width == sf_width && height == sf_height)
    return;

  if (pixel_surface)
    {
      cairo_surface_destroy (pixel_surface);
      munmap (pixels, sf_width*sf_height*4);
      free (stable_pixels);
    }

  pixels = mmap (NULL, width*height*4, PROT_READ | PROT_WRITE, MAP_SHARED,
                 PIXELS_FD, 0);
  stable_pixels = malloc(width*height*4);

  if (pixels == MAP_FAILED || stable_pixels == NULL)
    exitf (1, "pixels map: %m");

  sf_width = width;
  sf_height = height;
  pixel_surface = cairo_image_surface_create_for_data((unsigned char *)stable_pixels,
                                                      CAIRO_FORMAT_RGB24,
                                                      WIDTH, HEIGHT, WIDTH*4);
}

const char *debug_argv[20];
const char *prog;
char **args;

void
init_pipes ()
{
  int commands[2];
  int events[2];

  if (pipe (commands) < 0)
    exitf (1, "command pipe: %m");

  commands_write_fd = commands[1];
  commands_read_fd = commands[0];

  if (pipe (events) < 0)
    exitf (1, "events pipe: %m");

  events_write_fd = events[1];
  events_read_fd = events[0];
}

int pid;

void spawn();

gboolean
process_command (GIOChannel *chan, GIOCondition cond, gpointer userdata)
{
  struct command cmd;

  int n = read (commands_read_fd, &cmd, sizeof (cmd));
  if (n == 0) {
    int status;

    close (events_write_fd);
    close (commands_read_fd);
    waitpid (pid, &status, 0);
    if (WIFEXITED(status) && WEXITSTATUS(status) == 77)
      {
        spawn();
        return FALSE;
      }
    else
      exit(0);
  } else if (n < 0)
    exitf (1, "read: %m");
  else if (n != sizeof (cmd))
    exitf (1, "short read: %d", n);
  else {
    // printf ("cmd %d %d %d\n", cmd.op, cmd.arg1, cmd.arg2);
    if (cmd.op == OP_CONF)
      configure (cmd.arg1, cmd.arg2);
    else if (cmd.op == OP_SHOW_AND_GET) {
      cairo_surface_flush (pixel_surface);
      memcpy(stable_pixels, pixels, sf_width*sf_height*4);
      cairo_surface_mark_dirty (pixel_surface);
      gtk_widget_queue_draw (GTK_WIDGET(drawing_area));
      want_event();
    }
  }

  return TRUE;
}

void spawn() {
  init_pipes ();

  pid = fork ();
  if (pid < 0)
    exitf (1, "fork: %m");

  if (pid == 0)
    {
      dup2 (commands_write_fd, COMMANDS_FD);
      dup2 (events_read_fd, EVENTS_FD);
      close (events_read_fd);
      close (events_write_fd);
      close (commands_read_fd);
      close (commands_write_fd);

      execvp (prog, args);
      write (2, "exec\n", 5);
      _exit (0);
    }

  close (commands_write_fd);
  close (events_read_fd);

  queue_event(EV_SIZE, 1280, 1024, 0, 0);
  want_event();

  GIOChannel *cmd_channel = g_io_channel_unix_new (commands_read_fd);
  g_io_add_watch (cmd_channel, G_IO_IN | G_IO_HUP, process_command, NULL);
}

int
input_from_keyval (guint keyval)
{
  switch (keyval)
    {
    case GDK_KEY_BackSpace:
      return -EV_KEY_BACKSPACE;
    case GDK_KEY_Tab:
      return -EV_KEY_TAB;
    case GDK_KEY_Return:
      return -EV_KEY_RETURN;
    case GDK_KEY_Escape:
      return -EV_KEY_ESCAPE;
    case GDK_KEY_Delete:
      return -EV_KEY_DELETE;
    case GDK_KEY_Left:
      return -EV_KEY_LEFT;
    case GDK_KEY_Up:
      return -EV_KEY_UP;
    case GDK_KEY_Right:
      return -EV_KEY_RIGHT;
    case GDK_KEY_Down:
      return -EV_KEY_DOWN;
    case GDK_KEY_Page_Up:
      return -EV_KEY_PAGE_UP;
    case GDK_KEY_Page_Down:
      return -EV_KEY_PAGE_DOWN;
    case GDK_KEY_End:
      return -EV_KEY_END;
    case GDK_KEY_Home:
      return -EV_KEY_BEGIN;
    default:
      if (keyval < 128)
        return keyval;
      else {
        // printf("? %x\n", keyval);
        return 0;
      }
    }
}

int
state_from_mod (GdkModifierType mod)
{
  int state = 0;
  if (mod & GDK_CONTROL_MASK)
    state |= EV_STATE_CONTROL;
  if (mod & GDK_ALT_MASK)
    state |= EV_STATE_META;
  return state;
}

/* Do we really have to gimp up a whole event controller to get access
   to raw mouse pointer and key events?  GtkEventControllerKey is good
   enough for keys, but how do we get raw mouse clicks out of
   GtkGestures...?

   And it's not even supported to derive from EventController?
 */

struct _GtkEventController
{
  GObject parent_instance;
};

struct _GtkEventControllerClass
{
  GObjectClass parent_class;
  void     (* set_widget)   (GtkEventController *controller,
                             GtkWidget          *widget);
  void     (* unset_widget) (GtkEventController *controller);
  gboolean (* handle_event) (GtkEventController *controller,
                             GdkEvent            *event,
                             double              x,
                             double              y);
  void     (* reset)        (GtkEventController *controller);
  void     (* handle_crossing) (GtkEventController    *controller,
                                //const GtkCrossingData *crossing,
                                double                 x,
                                double                 y);
  /*<private>*/
  /* Tells whether the event is filtered out, %TRUE makes
   * the event unseen by the handle_event vfunc.
   */
  gboolean (* filter_event) (GtkEventController *controller,
                             GdkEvent           *event);
  gpointer padding[10];
};

#define GTK_TYPE_EVENT_CONTROLLER_RAW         (gtk_event_controller_raw_get_type ())
#define GTK_EVENT_CONTROLLER_RAW(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GTK_TYPE_EVENT_CONTROLLER_RAW, GtkEventControllerRaw))
#define GTK_EVENT_CONTROLLER_RAW_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), GTK_TYPE_EVENT_CONTROLLER_RAW, GtkEventControllerRawClass))
#define GTK_IS_EVENT_CONTROLLER_RAW(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GTK_TYPE_EVENT_CONTROLLER_RAW))
#define GTK_IS_EVENT_CONTROLLER_RAW_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GTK_TYPE_EVENT_CONTROLLER_RAW))
#define GTK_EVENT_CONTROLLER_RAW_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), GTK_TYPE_EVENT_CONTROLLER_RAW, GtkEventControllerRawClass))

typedef struct _GtkEventControllerRaw GtkEventControllerRaw;
typedef struct _GtkEventControllerRawClass GtkEventControllerRawClass;

GType               gtk_event_controller_raw_get_type  (void) G_GNUC_CONST;

GtkEventController *gtk_event_controller_raw_new (void);

struct _GtkEventControllerRaw
{
  GtkEventController parent_instance;
};

struct _GtkEventControllerRawClass
{
  GtkEventControllerClass parent_class;
};

G_DEFINE_TYPE (GtkEventControllerRaw, gtk_event_controller_raw,
               GTK_TYPE_EVENT_CONTROLLER)

gboolean repeating;

static gboolean
gtk_event_controller_raw_handle_event (GtkEventController *controller,
                                       GdkEvent     *event,
                                       double x,
                                       double y)
{
  GdkEventType t = gdk_event_get_event_type(event);
  int state = state_from_mod (gdk_event_get_modifier_state (event));

  // The drawing area doesn't get key events, so let's attach to the window and adjust coordinates...
  y -= (gtk_widget_get_height(GTK_WIDGET(window)) - gtk_widget_get_height(GTK_WIDGET(drawing_area)));

  switch (t) {
  case GDK_KEY_PRESS:
    if (!(repeating && n_events_queued > 0))
      {
        int input = input_from_keyval (gdk_key_event_get_keyval (event));
        if (input)
          {
            // printf ("KEY %d ('%c') %d\n", input, input, state);
            queue_event (EV_INPUT, x, y, state, input);
          }
      }
    repeating = TRUE;
    break;
  case GDK_KEY_RELEASE:
    repeating = FALSE;
    break;
  case GDK_BUTTON_PRESS:
    // printf ("PRESS %d %d %d\n", gdk_button_event_get_button(event), (int)x, (int)y);
    queue_event (EV_INPUT, x, y, state, -EV_BTN_1_PRESS);
    break;
  case GDK_BUTTON_RELEASE:
    queue_event (EV_INPUT, x, y, state, -EV_BTN_1_RELEASE);
    break;
  case GDK_SCROLL:
    switch (gdk_scroll_event_get_direction (event)) {
    case GDK_SCROLL_UP:
      queue_event (EV_INPUT, x, y, state, -EV_SCR_UP);
      break;
    case GDK_SCROLL_DOWN:
      queue_event (EV_INPUT, x, y, state, -EV_SCR_DOWN);
      break;
    }
    break;
  case GDK_MOTION_NOTIFY:
    if (gdk_event_get_modifier_state (event) & GDK_BUTTON1_MASK)
      queue_event (EV_INPUT, x, y, state, -EV_MOVE);
    break;
  }

  return TRUE;
}

static void
gtk_event_controller_raw_class_init (GtkEventControllerRawClass *klass)
{
  GtkEventControllerClass *controller_class = GTK_EVENT_CONTROLLER_CLASS (klass);
  controller_class->handle_event = gtk_event_controller_raw_handle_event;
}

static void
gtk_event_controller_raw_init (GtkEventControllerRaw *controller)
{
}

GtkEventController *
gtk_event_controller_raw_new (void)
{
   return g_object_new (GTK_TYPE_EVENT_CONTROLLER_RAW, NULL);
}

void activate(GtkApplication *app,
              gpointer user_data)
{
  window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "Z");

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(drawing_area), draw_func, NULL, NULL);
  gtk_widget_set_size_request(GTK_WIDGET(drawing_area), WIDTH, HEIGHT);
  gtk_window_set_child(GTK_WINDOW(window), drawing_area);
  gtk_window_present(GTK_WINDOW(window));

  GtkEventController *event_controller = gtk_event_controller_raw_new ();
  gtk_widget_add_controller (GTK_WIDGET (window), event_controller);

  spawn();
}

void
usage ()
{
  exitf (1, "usage: fb [-g] CMD...");
}

int main(int argc, char **argv)
{
  int debug = 0;
  int i;

  init_pixels ();

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

  GtkApplication *app = gtk_application_new("org.example.DrawingArea", G_APPLICATION_DEFAULT_FLAGS);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  int status = g_application_run(G_APPLICATION(app), 1, argv);
  g_object_unref(app);
  return status;
}
