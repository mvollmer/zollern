/*
   Z0 -- Bootstrap assembler
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <fcntl.h>

#include <elf.h>
#include <obstack.h>

bool verbose = false;

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

void *
xmalloc (size_t size)
{
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

  code = xmalloc (1024*1024);
  code_offset = 0;
  code_max = 1024*1024;

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
emit_code_at (uint64_t offset, int size, uint64_t val)
{
  uint8_t *ptr = code + offset;

  if (size < 0)
    {
      size = -size;
      val -= code_start + offset + size;
    }

  while (size > 0)
    {
      *ptr++ = val & 0xFF;
      val >>= 8;
      size -= 1;
    }
}

void
emit_code (int size, uint64_t val)
{
  if (code_offset + abs (size) >= code_max)
    exitf (1, "too large, congrats");

  emit_code_at (code_offset, size, val);
  code_offset += abs (size);
}

/* Data

   XXX - should work exactly like code
*/

uint64_t data_start;
uint64_t data_offset;

void
start_data ()
{
  data_start = (code_start + code_offset + 0xFFF) & ~0xFFF;
  data_offset = 0;
}

/* ELF output */

void
dump (const char *out_name)
{
  uint64_t str_text = strtab_add (".text");
  uint64_t str_data = strtab_add (".data");
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
    int64_t inum;
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
inum (uint64_t inum)
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

uint64_t
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
      fprintf (f, "%lu", inum_val(e));
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
      fprintf (f, ")");
    }
  else if (is_end_of_file (e))
    fprintf (f, "<eof>");
  else
    fprintf (f, "<?>");
}

const char *in_name;
FILE *in_file;

void
read_open (const char *name)
{
  in_name = name;
  in_file = fopen (in_name, "r");
  if (in_file == NULL)
    exitf (1, "%s: %m", in_name);
}

#define token_size 1024
char token[token_size];
enum { punct_tok, sym_tok, string_tok, inum_tok, dnum_tok, eof_tok } token_kind;

int lineno = 1;

void
read_token ()
{
  token[0] = '\0';

  int c;
 again:
  while (isspace (c = fgetc (in_file)))
    {
      if (c == '\n')
        lineno++;
    }

  if (c == ';')
    {
      while (c = fgetc (in_file) != '\n')
        ;
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
  else if (isdigit (c) || c == '-')
    {
      int i = 0;
      token_kind = inum_tok;
      token[i++] = c;
      while (isdigit (c = fgetc (in_file)) || c == '.' || c == 'x' ||
             (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
        {
          token[i++] = c;
          if (c == '.')
            token_kind = dnum_tok;
        }
      token[i] = '\0';
      ungetc (c, in_file);
      if (strcmp (token, "-") == 0)
        token_kind = sym_tok;
    }
  else if (c != EOF)
    {
      int i = 0;
      token_kind = sym_tok;
      token[i++] = c;
      while ((c = fgetc (in_file)) != EOF
             && !isspace (c) && c != '(' && c != ')')
        token[i++] = c;
      token[i] = '\0';
      ungetc (c, in_file);
    }
  else
    token_kind = eof_tok;
}

exp *
read_exp_1 ()
{
  if (token_kind == eof_tok)
    return end_of_file ();
  else if (token_kind == inum_tok)
    return inum (strtol (token, NULL, 0));
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
builtin_inum_binop (exp *form, int (*combine)(int, int))
{
  int lit_val;
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
builtin_sum (exp *form)
{
  int plus (int a, int b) { return a + b; }
  return builtin_inum_binop (form, plus);
}

exp *
builtin_diff (exp *form)
{
  int minus (int a, int b) { return a - b; }
  return builtin_inum_binop (form, minus);
}

exp *
builtin_prod (exp *form)
{
  int times (int a, int b) { return a * b; }
  return builtin_inum_binop (form, times);
}

exp *
builtin_lsh (exp *form)
{
  int lsh (int a, int b) { return a << b; }
  return builtin_inum_binop (form, lsh);
}

exp *
builtin_rsh (exp *form)
{
  int rsh (int a, int b) { return a >> b; }
  return builtin_inum_binop (form, rsh);
}

exp *
builtin_and (exp *form)
{
  int and (int a, int b) { return a & b; }
  return builtin_inum_binop (form, and);
}

exp *
builtin_or (exp *form)
{
  int or (int a, int b) { return a | b; }
  return builtin_inum_binop (form, or);
}

typedef struct {
  const char *name;
  exp *(*func)(exp *form);
} builtin;

builtin builtins[] = {
  { "def", builtin_def },
  { "+",   builtin_sum },
  { "-",   builtin_diff },
  { "*",   builtin_prod },
  { "<<",  builtin_lsh },
  { ">>",  builtin_rsh },
  { "and", builtin_and },
  { "or",  builtin_or },
  NULL
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
      e = reverse (x);
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
compile_emitter (exp *e)
{
  if (is_form (e, "begin"))
    compile_emitters (rest (e));
  else if (is_sym (e))
    {
      sym_def_label (e, code_start + code_offset);
      symtab_add (sym_name (e), code_start + code_offset);
    }
  else if (is_pair (e) && is_inum (first (e)))
    {
      int size = inum_val (first (e));
      exp *vals = rest (e);
      while (is_pair (vals))
        {
          exp *v = first (vals);
          if (is_inum (v))
            emit_code (size, inum_val (v));
          else if (is_string (v))
            {
              for (char *c = string_chars (v); *c; c++)
                emit_code (size, *c);
            }
          else
            {
              delay_emitter (code_offset, size, v);
              emit_code (size, 0);
            }
          vals = rest (vals);
        }
    }
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
    compile_emitters (rest (e));
  else
    error (e, "syntax");
}

void
compile_delayeds ()
{
  for (delayed_emitter *d = delayed; d; d = d->link)
    {
      exp *v = expand (d->val);
      if (is_inum (v))
        emit_code_at (d->offset, d->size, inum_val (v));
      else
        error (v, "undefined");
    }
}

/* Main */

void
usage ()
{
  exitf (1, "Usage: z0 [--start VADDR] IN OUT");
}

void
main (int argc, char **argv)
{
  const char *in;
  const char *out;

  argv++;
  while (*argv && *argv[0] == '-')
    {
      if (strcmp (*argv, "--start") == 0 && *(argv+1) != NULL)
        {
          set_code_start (strtol (*(argv+1), NULL, 0));
          argv += 2;
        }
      else
        usage ();
    }

  if (*argv && *(argv+1))
    {
      in = *argv;
      out = *(argv+1);
    }
  else
    usage ();

  init_code ();
  init_exp ();
  init_builtins ();

  read_open (in);

  exp *e;
  while (!is_end_of_file (e = read_exp ()))
    compile_toplevel (expand (e));

  compile_delayeds ();

  dump (out);
  exit (0);
}
