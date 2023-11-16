/*
   Z0 -- Bootstrap assembler
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <fcntl.h>

#include <elf.h>
#include <obstack.h>

bool verbose = false;

const char *in_name;
int lineno;

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

size_t total_alloc = 0;

void *
xmalloc (size_t size)
{
  total_alloc += size;
  if (total_alloc > 1<<30)
    exitf (1, "out of memory");

  void *mem = malloc (size);
  if (mem == 0)
    exitf (1, "Out of memory");
  return mem;
}

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Code */

uint8_t *code;
uint64_t code_offset;
uint64_t code_max;

struct obstack symtab_obs;
struct obstack strtab_obs;

struct file {
  Elf64_Ehdr h;
  Elf64_Phdr ph[2];
  Elf64_Shdr sh[4];
};

uint64_t code_vaddr;
uint64_t code_start;

void
set_code_start (uint64_t start)
{
  code_vaddr = start;
  code_start = code_vaddr + sizeof(struct file);
}

uint64_t
strtab_add (const char *str)
{
  uint64_t offset = obstack_object_size (&strtab_obs);
  obstack_grow0 (&strtab_obs, str, strlen (str));
  return offset;
}

void
symtab_add_raw (Elf64_Sym *sym)
{
  obstack_grow (&symtab_obs, sym, sizeof (Elf64_Sym));
}

void
symtab_add (const char *name, uint64_t value)
{
  Elf64_Sym sym;
  sym.st_name = strtab_add (name);
  sym.st_info = ELF64_ST_INFO (STB_GLOBAL, STT_NOTYPE);
  sym.st_other = STV_DEFAULT;
  sym.st_shndx = 1;
  sym.st_value = value;
  sym.st_size = 0;
  symtab_add_raw (&sym);
}

void
init_code()
{
  if (code_start == 0)
    set_code_start (0x40000000);

  code_max = 1024*1024 - (code_start - code_vaddr);
  code = xmalloc (code_max);
  code_offset = 0;

  obstack_init (&symtab_obs);
  obstack_init (&strtab_obs);

  strtab_add ("");
  Elf64_Sym sym;
  sym.st_name = 0;
  sym.st_info = 0;
  sym.st_other = 0;
  sym.st_shndx = 0;
  sym.st_value = 0;
  sym.st_size = 0;
  symtab_add_raw (&sym);

  sym.st_name = 0;
  sym.st_info = ELF64_ST_INFO (STB_LOCAL, STT_SECTION);
  sym.st_other = STV_DEFAULT;
  sym.st_shndx = 1;
  sym.st_value = code_start;
  sym.st_size = 0;
  symtab_add_raw (&sym);
}

void
emit_code_at (uint64_t offset, int size, int64_t val)
{
  int64_t orig_val;
  int orig_size;

  uint8_t *ptr = code + offset;

  if (size < 0)
    {
      size = -size;
      val -= code_start + offset + size;
    }

  orig_val = val;
  orig_size = size;

  while (size > 0)
    {
      *ptr++ = val & 0xFF;
      val >>= 8;
      size -= 1;
    }

  if (val != 0 && val != -1)
    fprintf (stderr, "%s:%d: warning: %ld out of range for %d bytes\n",
             in_name, lineno, orig_val, orig_size);
}

void
emit_code (int size, int64_t val)
{
  if (code_offset + abs (size) >= code_max)
    exitf (1, "too large, congrats");

  emit_code_at (code_offset, size, val);
  code_offset += abs (size);
}

/* Data
*/

uint64_t data_start;
uint64_t data_offset;

void
init_data ()
{
  if (data_start == 0)
    data_start = (code_start + code_max + 0xFFF) & ~0xFFF;

  data_offset = 0;
}

void
alloc_data (uint64_t size)
{
  data_offset += size;
  data_offset = (data_offset + 7) & ~7;
}

/* ELF output */

void
dump (const char *out_name)
{
  uint64_t str_text = strtab_add (".text");
  uint64_t str_strtab = strtab_add (".strtab");
  uint64_t str_symtab = strtab_add (".symtab");

  uint64_t code_size = code_offset;
  uint64_t data_size = data_offset;

  uint64_t symtab_offset = sizeof (struct file) + code_size;
  uint64_t symtab_size = obstack_object_size (&symtab_obs);
  char *symtab = obstack_finish (&symtab_obs);

  uint64_t strtab_offset = symtab_offset + symtab_size;
  uint64_t strtab_size = obstack_object_size (&strtab_obs);
  char *strtab = obstack_finish (&strtab_obs);

  struct file f;

  memset (f.h.e_ident, 0, sizeof (f.h.e_ident));
  f.h.e_ident[EI_MAG0] = ELFMAG0;
  f.h.e_ident[EI_MAG1] = ELFMAG1;
  f.h.e_ident[EI_MAG2] = ELFMAG2;
  f.h.e_ident[EI_MAG3] = ELFMAG3;
  f.h.e_ident[EI_CLASS] = ELFCLASS64;
  f.h.e_ident[EI_DATA] = ELFDATA2LSB;
  f.h.e_ident[EI_VERSION] = EV_CURRENT;
  f.h.e_ident[EI_OSABI] = ELFOSABI_NONE;
  f.h.e_ident[EI_ABIVERSION] = 0;
  f.h.e_type = ET_EXEC;
  f.h.e_machine = EM_X86_64;
  f.h.e_version = EV_CURRENT;
  f.h.e_entry = code_start;
  f.h.e_phoff = offsetof (struct file, ph[0]);
  f.h.e_shoff = offsetof (struct file, sh[0]);
  f.h.e_flags = 0;
  f.h.e_ehsize = sizeof (f.h);
  f.h.e_phentsize = sizeof (f.ph[0]);
  f.h.e_phnum = sizeof (f.ph) / sizeof (f.ph[0]);
  f.h.e_shentsize = sizeof (f.sh[0]);
  f.h.e_shnum = sizeof (f.sh) / sizeof (f.sh[0]);
  f.h.e_shstrndx = 2;

  f.ph[0].p_type = PT_LOAD;
  f.ph[0].p_flags = PF_X | PF_R;
  f.ph[0].p_offset = 0;
  f.ph[0].p_vaddr = code_vaddr;
  f.ph[0].p_paddr = code_vaddr;
  f.ph[0].p_filesz = sizeof(f) + code_size;
  f.ph[0].p_memsz = sizeof(f) + code_size;
  f.ph[0].p_align = 0;

  f.ph[1].p_type = PT_LOAD;
  f.ph[1].p_flags = PF_R | PF_W;
  f.ph[1].p_offset = 0;
  f.ph[1].p_vaddr = data_start;
  f.ph[1].p_paddr = data_start;
  f.ph[1].p_filesz = 0;
  f.ph[1].p_memsz = data_size;
  f.ph[1].p_align = 0;

  f.sh[0].sh_name = 0;
  f.sh[0].sh_type = SHT_NULL;
  f.sh[0].sh_flags = 0;
  f.sh[0].sh_addr = 0;
  f.sh[0].sh_offset = 0;
  f.sh[0].sh_size = 0;
  f.sh[0].sh_link = 0;
  f.sh[0].sh_info = 0;
  f.sh[0].sh_addralign = 0;
  f.sh[0].sh_entsize = 0;

  f.sh[1].sh_name = str_text;
  f.sh[1].sh_type = SHT_PROGBITS;
  f.sh[1].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
  f.sh[1].sh_addr = code_vaddr + sizeof (f);
  f.sh[1].sh_offset = sizeof (f);
  f.sh[1].sh_size = code_size;
  f.sh[1].sh_link = 0;
  f.sh[1].sh_info = 0;
  f.sh[1].sh_addralign = 4;
  f.sh[1].sh_entsize = 0;

  f.sh[2].sh_name = str_strtab;
  f.sh[2].sh_type = SHT_STRTAB;
  f.sh[2].sh_flags = 0;
  f.sh[2].sh_addr = 0;
  f.sh[2].sh_offset = strtab_offset;
  f.sh[2].sh_size = strtab_size;
  f.sh[2].sh_link = 0;
  f.sh[2].sh_info = 0;
  f.sh[2].sh_addralign = 1;
  f.sh[2].sh_entsize = 0;

  f.sh[3].sh_name = str_symtab;
  f.sh[3].sh_type = SHT_SYMTAB;
  f.sh[3].sh_flags = 0;
  f.sh[3].sh_addr = 0;
  f.sh[3].sh_offset = symtab_offset;
  f.sh[3].sh_size = symtab_size;
  f.sh[3].sh_link = 2;
  f.sh[3].sh_info = 2;
  f.sh[3].sh_addralign = 8;
  f.sh[3].sh_entsize = sizeof (Elf64_Sym);

  int fd = creat (out_name, 0777);

  if (fd < 0
      || write (fd, &f, sizeof(f)) != sizeof(f)
      || write (fd, code, code_size) != code_size
      || write (fd, symtab, symtab_size) != symtab_size
      || write (fd, strtab, strtab_size) != strtab_size
      || close (fd) < 0)
    exitf(1, "%s: %m", out_name);

  free (code);
  obstack_free (&symtab_obs, NULL);
  obstack_free (&strtab_obs, NULL);
}

/* Lists */

typedef struct symdef symdef;

typedef __int128 inum_t;

typedef struct exp {
  enum {
    pair_exp, sym_exp, string_exp, inum_exp, dnum_exp, singleton_exp
  } type;
  union {
    struct {
      struct exp *first;
      struct exp *rest;
    };
    struct {
      char *sym;
      struct exp *link;
      struct symdef *def;
    };
    inum_t inum;
    double dnum;
    char *string;
  };
} exp;

typedef exp *(*builtin_t)(exp *form);

struct symdef {
  enum {
    label_symdef, builtin_symdef, macros_symdef
  } type;
  union {
    uint64_t label;
    builtin_t builtin;
    exp *macros;
  };
};

void error (exp *e, const char *msg);

exp nil_obj, eof_obj;

struct obstack exp_obs;

struct obstack symbol_obs;
#define symbol_hash_size 1023
exp *symbol_hash[symbol_hash_size];

void
init_exp ()
{
  obstack_init (&exp_obs);
  obstack_init (&symbol_obs);
  nil_obj.type = eof_obj.type = singleton_exp;
}

void
reset_exp()
{
  obstack_free (&exp_obs, NULL);
  obstack_init (&exp_obs);
}

void *
alloc_exp_data (size_t n)
{
  return obstack_alloc (&exp_obs, n);
}

exp *
alloc_exp ()
{
  return alloc_exp_data (sizeof(exp));
}

exp *
nil ()
{
  return &nil_obj;
}

bool
is_nil (exp *e)
{
  return e == &nil_obj;
}

exp *
end_of_file ()
{
  return &eof_obj;
}

bool
is_end_of_file (exp *e)
{
  return e == &eof_obj;
}

exp *
cons (exp *first, exp *rest)
{
  exp *e = alloc_exp ();
  e->type = pair_exp;
  e->first = first;
  e->rest = rest;
  return e;
}

bool
is_pair (exp *e)
{
  return e->type == pair_exp;
}

exp *
first (exp *e)
{
  return e->first;
}

exp *
rest (exp *e)
{
  return e->rest;
}

exp *
second (exp *e)
{
  return first (rest (e));
}

exp *
third (exp *e)
{
  return first (rest (rest (e)));
}

exp *
inum (inum_t inum)
{
  exp *e = alloc_exp ();
  e->type = inum_exp;
  e->inum = inum;
  return e;
}

bool
is_inum (exp *e)
{
  return e->type == inum_exp;
}

inum_t
inum_val (exp *e)
{
  return e->inum;
}

exp *
dnum (double dnum)
{
  exp *e = alloc_exp ();
  e->type = dnum_exp;
  e->dnum = dnum;
  return e;
}

bool
is_dnum (exp *e)
{
  return e->type == dnum_exp;
}

double
dnum_val (exp *e)
{
  return e->dnum;
}

exp *
sym (const char *sym)
{
  unsigned int hash = 5381;

  const char *p = sym;
  while (*p)
    hash = (hash*33) ^ *p++;
  hash = hash % symbol_hash_size;

  for (exp *e = symbol_hash[hash]; e; e = e->link)
    if (strcmp (e->sym, sym) == 0)
      return e;

  exp *e = alloc_exp ();
  e->type = sym_exp;
  e->sym = obstack_copy0 (&symbol_obs, sym, strlen (sym));
  e->link = symbol_hash[hash];
  e->def = NULL;
  symbol_hash[hash] = e;

  return e;
}

void
sym_foreach (void (*func) (exp *sym))
{
  for (int i = 0; i < symbol_hash_size; i++)
    for (exp *e = symbol_hash[i]; e; e = e->link)
      func (e);
}

bool
is_sym (exp *e)
{
  return e && e->type == sym_exp;
}

char *
sym_name (exp *e)
{
  return e->sym;
}

exp *
gensym (char *tag)
{
  static int counter;
  char name[80];
  snprintf(name, 80, "%d-%s", counter++, tag? tag : "gensym");
  return sym(name);
}

void
sym_undef (exp *e)
{
  e->def = NULL;
}

void
sym_def_builtin (exp *e, builtin_t builtin)
{
  if (e->def)
    error (e, "redefined");
  symdef *d = alloc_exp_data (sizeof (symdef));
  d->type = builtin_symdef;
  d->builtin = builtin;
  e->def = d;
}

bool
sym_is_builtin (exp *e)
{
  return e->def && e->def->type == builtin_symdef;
}

builtin_t
sym_builtin (exp *e)
{
  return e->def->builtin;
}

void
sym_def_label (exp *e, uint64_t label)
{
  if (e->def)
    error (e, "redefined");
  symdef *d = alloc_exp_data (sizeof (symdef));
  d->type = label_symdef;
  d->label = label;
  e->def = d;
}

bool
sym_is_label (exp *e)
{
  return e->def && e->def->type == label_symdef;
}

uint64_t
sym_label (exp *e)
{
  return e->def->label;
}

void
sym_def_add_macro (exp *e, exp *macro)
{
  if (e->def == NULL)
    {
      symdef *d = alloc_exp_data (sizeof (symdef));
      d->type = macros_symdef;
      d->macros = nil ();
      e->def = d;
    }
  else if (e->def->type != macros_symdef)
    error (e, "redefined");

  e->def->macros = cons (macro, e->def->macros);
}

bool
sym_is_macros (exp *e)
{
  return e->def && e->def->type == macros_symdef;
}

exp *
sym_macros (exp *e)
{
  return e->def->macros;
}

exp *
string (char *str)
{
  exp *e = alloc_exp ();
  e->type = string_exp;
  e->string = alloc_exp_data (strlen (str) + 1);
  strcpy (e->string, str);
  return e;
}

bool
is_string (exp *e)
{
  return e && e->type == string_exp;
}

char *
string_chars (exp *e)
{
  return e->string;
}

exp *
reverse_onto (exp *e, exp *tail)
{
  exp *r = tail;
  while (is_pair (e))
    {
      exp *n = e->rest;
      e->rest = r;
      r = e;
      e = n;
    }
  return r;
}

exp *
reverse (exp *e)
{
  return reverse_onto (e, nil ());
}

int
len (exp *e)
{
  int l = 0;
  while (is_pair (e))
    {
      e = rest (e);
      l += 1;
    }
  return l;
}

/* Reading and writing */

void
write_exp (FILE *f, exp *e)
{
  if (is_sym (e))
    {
      // XXX - escape things properly
      fprintf (f, "%s", sym_name (e));
    }
  else if (is_string (e))
    {
      // XXX - escape things properly
      fprintf (f, "\"%s\"", string_chars (e));
    }
  else if (is_inum (e))
    {
      inum_t val = inum_val(e);
      if (val >= LONG_MIN && val <= LONG_MAX)
        fprintf (f, "%ld", (long)val);
      else if (val >= 0 && val <= ULONG_MAX)
        fprintf (f, "%lu", (unsigned long)val);
      else
        fprintf (f, "<out-of-range>");
    }
  else if (is_dnum (e))
    {
      fprintf (f, "%g", dnum_val(e));
    }
  else if (is_nil (e) || is_pair (e))
    {
      bool need_space = false;
      fprintf (f, "(");
      while (is_pair (e))
        {
          if (need_space)
            fprintf (f, " ");
          write_exp (f, e->first);
          e = e->rest;
          need_space = true;
        }
      if (!is_nil (e))
        {
          fprintf (f, " . ");
          write_exp (f, e);
        }
      fprintf (f, ")");
    }
  else if (is_end_of_file (e))
    fprintf (f, "<eof>");
  else
    fprintf (f, "<?>");
}

FILE *in_file;
bool at_bol;

void
read_open (const char *name)
{
  in_name = name;
  in_file = fopen (in_name, "r");
  lineno = 1;
  if (in_file == NULL)
    exitf (1, "%s: %m", in_name);
  at_bol = true;
}

void
read_close ()
{
  fclose (in_file);
}

#define token_size 1024
char token[token_size];
enum { punct_tok, sym_tok, string_tok, inum_tok, dnum_tok, eof_tok } token_kind;

int
isdelimiter (int c)
{
  return isspace (c) || c == '(' || c == ')';
}

int
isnumstartchar (int c)
{
  return isdigit (c) || c == '-';
}

int
isnumrestchar (int c)
{
  return (isdigit (c) || c == '.' || c == 'x' ||
          (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

void
read_token ()
{
  token[0] = '\0';

  int c;
 again:
  while (isspace (c = fgetc (in_file)))
    {
      if (c == '\n')
        {
          lineno++;
          at_bol = true;
        }
      else
        at_bol = false;
    }

  if (c == '@' && at_bol)
    {
    again_section:
      c = fgetc (in_file);
      at_bol = false;
      if (c == '=')
        goto again;

      while (!(c== '@' && at_bol))
        {
          if (c == '\n')
            {
              lineno++;
              at_bol = true;
            }
          else
            at_bol = false;
          c = fgetc (in_file);
          if (c == EOF)
            break;
        }

      if (c != EOF)
        goto again_section;
    }

  if (c == ';')
    {
      while ((c = fgetc (in_file)) != '\n')
        ;
      lineno++;
      goto again;
    }

  if (c == '(' || c == ')')
    {
      token[0] = c;
      token[1] = '\0';
      token_kind = punct_tok;
    }
  else if (c == '\'' || c == '"')
    {
      char quote = c;
      bool escape_next = false;
      int i = 0;
      while ((c = fgetc (in_file)) != EOF && (c != quote || escape_next))
        {
          if (c == '\n')
            lineno++;
          if (c == '\\' && !escape_next)
            escape_next = true;
          else
            {
               if (escape_next)
                 {
                   switch (c) {
                   case 'n':
                     c = '\n';
                     break;
                   case '"':
                     c = '"';
                     break;
                   default:
                     exitf (1, "unknown string escape '%c'", c);
                     break;
                   }
                 }

              token[i++] = c;
              escape_next = false;
            }
        }
      token[i] = '\0';
      token_kind = (quote == '\''? sym_tok : string_tok);
    }
  else if (c != EOF)
    {
      int i = 0;
      token_kind = inum_tok;
      while (c != EOF && !isdelimiter (c))
        {
          if (!((i == 0)? isnumstartchar (c) : isnumrestchar (c)))
            token_kind = sym_tok;
          if (c == '.' && token_kind == inum_tok)
            token_kind = dnum_tok;
          token[i++] = c;
          c = fgetc (in_file);
        }
      token[i] = '\0';
      ungetc (c, in_file);
      if (strcmp (token, "-") == 0)
        token_kind = sym_tok;
    }
  else
    token_kind = eof_tok;
}

inum_t
strtoinum (const char *str)
{
  const char *ptr = str;
  int base = 10;
  int sign = 1;
  inum_t val;

  if (ptr[0] == '-')
    {
      sign = -1;
      ptr += 1;
    }

  if (ptr[0] == '0' && ptr[1] == 'x')
    {
      base = 16;
      ptr += 2;
    }

  val = 0;
  while (*ptr)
    {
      int digit;
      if (base == 16 && *ptr >= 'a' && *ptr <= 'f')
        digit = 10 + *ptr - 'a';
      else if (base == 16 && *ptr >= 'A' && *ptr <= 'F')
        digit = 10 + *ptr - 'A';
      else if (*ptr >= '0' && *ptr <= '9')
        digit = *ptr - '0';
      else
        exitf(0, "invalid number %s", str);

      val = val*base + digit;
      ptr++;
    }

  return sign*val;
}

exp *
read_exp_1 ()
{
  if (token_kind == eof_tok)
    return end_of_file ();
  else if (token_kind == inum_tok)
    return inum (strtoinum (token));
  else if (token_kind == dnum_tok)
    return dnum (strtod (token, NULL));
  else if (token_kind == sym_tok)
    return sym (token);
  else if (token_kind == string_tok)
    return string (token);
  else if (token_kind == punct_tok && token[0] == '(')
    {
      exp *e = nil ();
      while (true) {
        read_token ();
        if (token_kind == punct_tok && token[0] == ')')
          return reverse (e);
        else if (token_kind == sym_tok && strcmp (token, ".") == 0)
          {
            read_token ();
            e = reverse_onto (e, read_exp_1 ());
            read_token ();
            if (token_kind != punct_tok || token[0] != ')')
              exitf(1, "junk after tail: %s", token);
            return e;
          }
        else if (token_kind == eof_tok)
          exitf (1, "unterminated list");
        e = cons (read_exp_1 (), e);
      }
    }
  else
    exitf(1, "%d: unexpected token '%s'", lineno, token);
}

exp *
read_exp ()
{
  read_token ();
  return read_exp_1 ();
}

void
error (exp *form, const char *msg)
{
  fprintf (stderr, "%s:%d: ", in_name, lineno);
  write_exp (stderr, form);
  fprintf (stderr, "\n");
  exitf (1, "error: %s", msg);
}

bool
is_form (exp *e, const char *sym)
{
  return (is_pair (e) &&
          is_sym (first (e)) && strcmp (sym_name (first (e)), sym) == 0);
}

void
parse_form (exp *e, int mandatory, int optional, ...)
{
  va_list ap;
  va_start (ap, optional);

  exp *args = rest (e);
  while (mandatory > 0 || optional > 0)
    {
      exp **argp = va_arg (ap, exp **);
      if (!is_pair (args))
        {
          if (mandatory > 0)
            error (e, "syntax");
          else
            {
              *argp = NULL;
              optional--;
            }
        }
      else
        {
          if (mandatory > 0)
            mandatory--;
          else
            optional--;
          *argp = first (args);
          args = rest (args);
        }
    }

  if (is_pair (args))
    error (e, "syntax");
}

/* Macros and builtins
 */

exp *expand (exp *e);

exp *
builtin_def (exp *form)
{
  exp *head = first (rest (form));
  exp *body = rest (rest (form));
  exp *name;

  if (is_pair (head))
    name = first (head);
  else
    name = head;

  if (is_pair (body) && !is_pair (rest (body)))
    body = first (body);
  else
    body = cons (sym ("begin"), body);

  if (!is_sym (name))
    error (form, "syntax");

  sym_def_add_macro (name, cons (head, body));

  return cons (sym ("begin"), nil());
}

exp *
builtin_sel (exp *form)
{
  exp *e = rest (form);

  while (is_pair (e)
         && is_pair (rest (e)))
    {
      exp *cond = expand (first (e));
      if (is_inum (cond) && inum_val (cond) != 0)
        return expand (first (rest (e)));
      e = rest (rest (e));
    }

  if (is_pair (e))
    return expand (first (e));

  error (form, "unselected");
}

exp *
builtin_peek (exp *form)
{
  if (!is_pair (form)
      || !is_pair (rest (form))
      || !is_nil (rest (rest (form))))
    error (form, "syntax");

  exp *a = expand (first (rest (form)));
  write_exp (stdout, first (rest (form)));
  fprintf (stdout, " -> ");
  write_exp (stdout, a);
  fprintf (stdout, "\n");
  return a;
}

exp *
builtin_inum_mulop (exp *form, inum_t (*combine)(inum_t, inum_t))
{
  inum_t lit_val;
  bool got_lit = false;
  exp *non_lit = nil ();

  exp *e = rest (form);
  while (is_pair (e))
    {
      exp *v = expand (first (e));
      if (is_inum (v))
        {
          if (!got_lit)
            {
              lit_val = inum_val (v);
              got_lit = true;
            }
          else
            lit_val = combine (lit_val, inum_val (v));
        }
      else
        non_lit = cons (v, non_lit);
      e = rest (e);
    }

  if (is_nil (non_lit) && got_lit)
    return inum (lit_val);
  else if (got_lit)
    return cons (first (form),
                 cons (inum (lit_val),
                       reverse (non_lit)));
  else
    return form;
}

exp *
builtin_inum_binop (exp *form, inum_t (*op)(inum_t, inum_t))
{
  if (!is_pair (form)
      || !is_pair (rest (form))
      || !is_pair (rest (rest (form)))
      || !is_nil (rest (rest (rest (form)))))
    error (form, "syntax");

  exp *a = expand (first (rest (form)));
  exp *b = expand (first (rest (rest (form))));

  if (is_inum (a) && is_inum (b))
    {
      return inum (op (inum_val (a), inum_val (b)));
    }
  else
    return form;
}

exp *
builtin_inum_unaop (exp *form, inum_t (*op)(inum_t))
{
  if (!is_pair (form)
      || !is_pair (rest (form))
      || !is_nil (rest (rest (form))))
    error (form, "syntax");

  exp *a = expand (first (rest (form)));

  if (is_inum (a))
    return inum (op (inum_val (a)));
  else
    return form;
}

#define DEFMULOP(NAME, OP)                               \
  exp *                                                  \
  builtin_##NAME (exp *form)                             \
  {                                                      \
    inum_t NAME (inum_t a, inum_t b) { return a OP b; }  \
    return builtin_inum_mulop (form, NAME);              \
  }

#define DEFBINOP(NAME, OP)                               \
  exp *                                                  \
  builtin_##NAME (exp *form)                             \
  {                                                      \
    inum_t NAME (inum_t a, inum_t b) { return a OP b; }  \
    return builtin_inum_binop (form, NAME);              \
  }

#define DEFUNAOP(NAME, OP)                               \
  exp *                                                  \
  builtin_##NAME (exp *form)                             \
  {                                                      \
    inum_t NAME (inum_t a) { return OP a; }              \
    return builtin_inum_unaop (form, NAME);              \
  }

DEFMULOP(sum, +)
DEFMULOP(diff, -)
DEFMULOP(prod, *)
DEFMULOP(bitand, &)
DEFMULOP(bitor, |)
DEFMULOP(bitxor, ^)
DEFUNAOP(bitnot, ~)
DEFBINOP(lsh, <<)
DEFBINOP(rsh, >>)

DEFMULOP(and, &&)
DEFMULOP(or, ||)
DEFUNAOP(not, !)

DEFBINOP(eq, ==)
DEFBINOP(ne, !=)
DEFBINOP(lt, <)
DEFBINOP(le, <=)
DEFBINOP(gt, >)
DEFBINOP(ge, >=)

typedef struct {
  const char *name;
  exp *(*func)(exp *form);
} builtin;

builtin builtins[] = {
  { "def", builtin_def },
  { "sel", builtin_sel },
  { "+",   builtin_sum },
  { "-",   builtin_diff },
  { "*",   builtin_prod },
  { "<<",  builtin_lsh },
  { ">>",  builtin_rsh },
  { "&",   builtin_bitand },
  { "|",   builtin_bitor },
  { "^",   builtin_bitxor },
  { "~",   builtin_bitnot },
  { "&&",  builtin_and },
  { "||",  builtin_or },
  { "!",   builtin_not },
  { "==",  builtin_eq },
  { "!=",  builtin_ne },
  { "<",   builtin_lt },
  { "<=",  builtin_le },
  { ">",   builtin_gt },
  { ">=",  builtin_ge },
  { "peek", builtin_peek },
  { NULL }
};

void
init_builtins ()
{
  for (builtin *b = builtins; b->name; b++)
    {
      exp *s = sym (b->name);
      sym_def_builtin (s, b->func);
    }
}

bool
is_var (exp *e)
{
  return is_sym (e) && sym_name(e)[0] == '?';
}

exp *
lookup_var (exp *vars, exp *var)
{
  while (is_pair (vars))
    {
      if (first (first (vars)) == var)
        return rest (first (vars));
      vars = rest (vars);
    }

  return end_of_file ();
}

exp *
define_var (exp *vars, exp *var, exp *val)
{
  return cons (cons (var, val), vars);
}

exp *
match1 (exp *pattern, exp *vars, exp *e)
{
  if (is_var (pattern))
    {
      if (!is_end_of_file (lookup_var (vars, pattern)))
        error (pattern, "redefinition");
      vars = define_var (vars, pattern, e);
    }
  else if (is_pair (pattern))
    {
      if (!is_pair (e))
        return end_of_file ();

      vars = match1 (first (pattern), vars, first (e));
      if (is_end_of_file (vars))
        return vars;

      vars = match1 (rest (pattern), vars, rest (e));
      if (is_end_of_file (vars))
        return vars;
    }
  else if (is_nil (pattern))
    {
      if (!is_nil (e))
        return end_of_file ();
    }
  else if (is_sym (pattern))
    {
      if (pattern != e)
        return end_of_file ();
    }
  else if (is_inum (pattern))
    {
      if (!is_inum (e) || inum_val (pattern) != inum_val (e))
        return end_of_file ();
    }
  else
    return end_of_file ();

  return vars;
}

exp *
match (exp *pattern, exp *e)
{
  return match1 (pattern, nil (), e);
}

exp *
subst (exp *body, exp *vars)
{
  if (is_var (body))
    {
      if (sym_name(body)[1] == '?')
        return sym (sym_name(body) + 1);

      exp *val = lookup_var (vars, body);
      if (is_end_of_file (val))
        error (body, "undefined");
      return val;
    }
  else if (is_pair (body))
    {
      return cons (subst (first (body), vars),
                   subst (rest (body), vars));
    }
  else
    return body;
}

bool expand_no_undefined_locals = 0;

exp *
expand1 (exp *e)
{
  if (is_pair (e) && is_sym (first (e)))
    {
      if (sym_is_builtin (first (e)))
        return (sym_builtin (first (e))) (e);
    }

  if (is_pair (e))
    {
      exp *x = nil ();
      while (is_pair (e))
        {
          x = cons (expand (first (e)), x);
          e = rest (e);
        }
      e = reverse_onto (x, expand (e));
    }
  else if (is_sym (e) && sym_is_label (e))
    return inum (sym_label (e));

  exp *name;
  if (is_pair (e))
    name = first (e);
  else
    name = e;

  if (is_sym (name) && sym_is_macros (name))
    {
      for (exp *m = sym_macros (name); is_pair (m); m = rest (m))
        {
          exp *pattern = first (first (m));
          exp *body = rest (first (m));

          exp *vars = match (pattern, e);
          if (!is_end_of_file (vars))
            return expand (subst (body, vars));
        }
    }

  if (expand_no_undefined_locals && is_sym (e) && sym_name (e)[0] == '.')
    error (e, "undefined");

  return e;
}

char *
blanks(int l)
{
  static char b[] = "                                                          ";
  return b + (sizeof(b)-l-1);
}

exp *
expand (exp *e)
{
  if (verbose)
    {
      static int l = 0;
      printf ("%s", blanks(l));
      write_exp (stdout, e);
      printf ("\n");
      l += 1;
      exp *x = expand1 (e);
      l -= 1;
      printf ("%s-> ", blanks(l));
      write_exp (stdout, x);
      printf ("\n");
      return x;
    }
  else
    return expand1 (e);
}

/* Assembler */

void compile_delayeds ();

void
toss_local_definitions ()
{
  void toss_local (exp *e)
  {
    if (sym_name(e)[0] == '.')
      sym_undef (e);
  }

  sym_foreach (toss_local);
}

typedef struct delayed_emitter {
  struct delayed_emitter *link;
  uint64_t offset;
  int size;
  exp *val;
} delayed_emitter;

delayed_emitter *delayed = NULL;

void
delay_emitter (uint64_t offset, int size, exp *val)
{
  delayed_emitter *d = xmalloc (sizeof (delayed_emitter));
  d->offset = offset;
  d->size = size;
  d->val = val;
  d->link = delayed;
  delayed = d;
}

void compile_emitters (exp *e);

void
compile_emitter_elements (int size, exp *elts)
{
  while (is_pair (elts))
    {
      exp *v = first (elts);
      if (is_inum (v))
        emit_code (size, inum_val (v));
      else if (is_string (v))
        {
          for (char *c = string_chars (v); *c; c++)
            emit_code (size, *c);
        }
      else if (is_form (v, "begin"))
        compile_emitter_elements (size, rest (v));
      else
        {
          delay_emitter (code_offset, size, v);
          emit_code (abs (size), 0);
        }
      elts = rest (elts);
    }
}

void
compile_emitter (exp *e)
{
  if (is_form (e, "begin"))
    compile_emitters (rest (e));
  else if (is_sym (e))
    {
      sym_def_label (e, code_start + code_offset);
      if (sym_name(e)[0] != '.')
        symtab_add (sym_name (e), code_start + code_offset);
    }
  else if (is_pair (e) && is_inum (first (e)))
    compile_emitter_elements (inum_val (first (e)), rest (e));
  else
    error (e, "syntax");
}

void
compile_emitters (exp *e)
{
  while (is_pair (e))
    {
      compile_emitter (first (e));
      e = rest (e);
    }
}

void compile_allocators (exp *e);

void
compile_allocator (exp *e)
{
  if (is_form (e, "begin"))
    compile_allocators (rest (e));
  else if (is_sym (e))
    {
      sym_def_label (e, data_start + data_offset);
      if (sym_name(e)[0] != '.')
        symtab_add (sym_name (e), data_start + data_offset);
    }
  else if (is_inum (e))
    alloc_data (inum_val (e));
  else
    error (e, "syntax");
}

void
compile_allocators (exp *e)
{
  while (is_pair (e))
    {
      compile_allocator (first (e));
      e = rest (e);
    }
}

void
compile_toplevel (exp *e)
{
  if (is_form (e, "begin"))
    {
      e = rest (e);
      while (is_pair (e))
        {
          compile_toplevel (first (e));
          e = rest (e);
        }
    }
  else if (is_form (e, "code"))
    {
      compile_emitters (rest (e));
      compile_delayeds ();
      toss_local_definitions ();
    }
  else if (is_form (e, "data"))
    {
      compile_allocators (rest (e));
      compile_delayeds ();
      toss_local_definitions ();
    }
  else
    error (e, "syntax");
}

void
compile_delayeds ()
{
  expand_no_undefined_locals = 1;

  delayed_emitter **dd = &delayed;
  while (*dd)
    {
      delayed_emitter *d = *dd;
      exp *v = expand (d->val);
      if (is_inum (v))
        {
          emit_code_at (d->offset, d->size, inum_val (v));
          *dd = d->link;
        }
      else
        dd = &(d->link);
    }

  expand_no_undefined_locals = 0;
}

void
check_delayeds ()
{
  if (delayed)
    error (delayed->val, "undefined");
}

/* Main */

void
usage ()
{
  exitf (1, "Usage: z0 IN... OUT");
}

void
process_file (const char *name)
{
  read_open (name);

  exp *e;
  while (!is_end_of_file (e = read_exp ()))
    compile_toplevel (expand (e));

  read_close ();
}

int
main (int argc, char **argv)
{
  argv++;
  if (*argv == NULL)
    usage ();

  init_code ();
  init_data ();
  init_exp ();
  init_builtins ();

  while (*argv && *(argv+1))
    {
      process_file (*argv);
      argv += 1;
    }

  check_delayeds ();

  printf ("allocated %ld bytes\n", total_alloc);

  dump (*argv);
  return 0;
}
