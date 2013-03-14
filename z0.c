/*
   Z0 -- Bootstrap language.

   The first language is very primitive: there are no types, no memory
   management, and only primitive control structures.  It is compiled
   to machine code by this program.

   This language is only used to write one program: A bad compiler for
   a better language (which is then used to compile a good compiler
   written in the better language for the better language, which is
   then used to compile itself).  As such, many low hanging fruits are
   left rotting.

 # Global definitions

   - (fun (SYMBOL SYMBOL...)
       LOCAL-DEFINITION...
       STATEMENT...)

   Function definition.  The first symbol is the name of the function,
   the rest are the names for the arguments.

   - (data SYMBOL (o EXPR...) (t EXPR...) (w EXPR...) (b EXPR...) ...)

   Data definition.

   - (const SYM EXPR)

   Constant definition.

 # Local definitions

   - (var SYMBOL EXPR)
   - (var SYMBOL)

   Variable definition.

 # Statements

   - SYMBOL

   A label.  (Note that labels can not appear between definitions.)

   - (goto SYMBOL [EXPR])

   Transfer control to the named label if the given expression is
   non-zero.  When the expression is omitted, control is transferred
   unconditionally.

   - (return EXPR)

   Return the value of EXPR from the current function.  If the end of
   a functions body is reached, the return value is 0.

   - (= SYMBOL EXPR)

   Assignment.  SYMBOL must refer to a variable.

   - (@ EXPR [EXPR] EXPR)
   - (t@ EXPR [EXPR] EXPR)
   - (w@ EXPR [EXPR] EXPR)
   - (b@ EXPR [EXPR] EXPR)

   Store a octa, tetra, wyde, or byte.  First expression is the
   address, the second or third is the value.  If the middle
   expression is present, it gives an offset.

 # Expressions

   - NUMBER

   A literal.

   - SYMBOL

   Variable, constant, function, or argument reference.  For a
   function or data definition, the value is the address of the object
   in memory.

   - (and EXPR...)
   - (or EXPR...)
   - (not EXPR)

   Boolean operators.  All expressions are evaluated.

   - (+ EXPR...)
   - (- EXPR...)
   - (* EXPR...)
   - (/ EXPR...)
   - (% EXPR...)

   Arithmetic on signed integers.

   - (u+ EXPR...)
   - (u- EXPR...)
   - (u* EXPR...)
   - (u/ EXPR...)
   - (u% EXPR...)

   Arithmetic on unsigned integers.

   - (f+ EXPR...)
   - (f- EXPR...)
   - (f* EXPR...)
   - (f/ EXPR...)
   - (fmod EXPR...)
   - (frem EXPR...)

   Arithmetic on double precision floats.

   - (& SYMBOL)

   Address of SYMBOL, which must refer to a variable.

   - (@ EXPR [EXPR])
   - (t@ EXPR [EXPR])
   - (w@ EXPR [EXPR])
   - (b@ EXPR [EXPR])

   Read the octa, tetra, wyde, or byte at the given address as a
   signed value.  The second expression, if present, gives an offset.

   - (ut@ EXPR [EXPR])
   - (uw@ EXPR [EXPR])
   - (ub@ EXPR [EXPR])

   Read the tetra, wyde, or byte at the given address as a unsigned
   value.

   - (syscall ...)

   Call into the kernel.

   - (EXPR EXPR...)

   Function call.  The first expression is the address of the function
   to call, the rest are the arguments.

*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include <elf.h>
#include <obstack.h>

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

/* Machine code */

uint8_t *code;
uint8_t *code_ptr;
uint8_t *code_end;
uint64_t code_offset;

struct obstack symtab_obs;
struct obstack strtab_obs;

struct file {
  Elf64_Ehdr h;
  Elf64_Phdr ph[2];
  Elf64_Shdr sh[4];
};

#define CODE_VADDR 0x40000000
uint64_t code_start = CODE_VADDR + sizeof(struct file);

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
symtab_add (const char *name, uint64_t offset)
{
  Elf64_Sym sym;
  sym.st_name = strtab_add (name);
  sym.st_info = ELF64_ST_INFO (STB_GLOBAL, STT_NOTYPE);
  sym.st_other = STV_DEFAULT;
  sym.st_shndx = 1;
  sym.st_value = code_start + offset;
  sym.st_size = 0;
  symtab_add_raw (&sym);
}

void
init_code()
{
  code = xmalloc (1024*1024);
  code_ptr = code;
  code_end = code + 1024*1024;
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
dump_code()
{
  uint64_t str_text = strtab_add (".text");
  uint64_t str_strtab = strtab_add (".strtab");
  uint64_t str_symtab = strtab_add (".symtab");

  uint64_t code_size = code_offset;

  uint64_t symtab_offset = sizeof (struct file) + code_size;
  uint64_t symtab_size = obstack_object_size (&symtab_obs);
  char *symtab = obstack_finish (&symtab_obs);

  uint64_t strtab_offset = symtab_offset + symtab_size;
  uint64_t strtab_size = obstack_object_size (&strtab_obs);
  char *strtab = obstack_finish (&strtab_obs);

  uint64_t vaddr = CODE_VADDR;

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
  f.h.e_entry = vaddr + sizeof (f);
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
  f.ph[0].p_flags = PF_X | PF_W | PF_R;
  f.ph[0].p_offset = 0;
  f.ph[0].p_vaddr = 0x1000;
  f.ph[0].p_paddr = 0x1000;
  f.ph[0].p_filesz = 0;
  f.ph[0].p_memsz = vaddr - 0x1000;
  f.ph[0].p_align = 0;

  f.ph[1].p_type = PT_LOAD;
  f.ph[1].p_flags = PF_X | PF_R;
  f.ph[1].p_offset = 0;
  f.ph[1].p_vaddr = vaddr;
  f.ph[1].p_paddr = vaddr;
  f.ph[1].p_filesz = sizeof(f) + code_size;
  f.ph[1].p_memsz = sizeof(f) + code_size;
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
  f.sh[1].sh_addr = vaddr + sizeof (f);
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

  if (write (1, &f, sizeof(f)) != sizeof(f)
      || write (1, code, code_size) != code_size
      || write (1, symtab, symtab_size) != symtab_size
      || write (1, strtab, strtab_size) != strtab_size)
    exitf(1, "Can't write code: %m");

  free (code);
  obstack_free (&symtab_obs, NULL);
  obstack_free (&strtab_obs, NULL);
}

void
emit_8 (uint8_t b)
{
  *code_ptr++ = b;
  if (code_ptr >= code_end)
    exitf (1, "too large, congrats");
  code_offset += 1;
}

void
emit_16 (uint16_t w)
{
  emit_8 (w & 0xFF);
  emit_8 (w >> 8);
}

void
emit_32 (uint32_t t)
{
  emit_16 (t & 0xFFFF);
  emit_16 (t >> 16);
}

void
emit_64 (uint64_t o)
{
  emit_32 (o & 0xFFFFFFFF);
  emit_32 (o >> 32);
}

uint64_t
emit_set (uint32_t t)
{
  emit_8 (0x48);
  emit_8 (0xC7);
  emit_8 (0xC0);
  uint64_t off = code_offset;
  emit_32 (t);
  return off;
}

void
emit_syscall ()
{
  emit_8 (0x0F);
  emit_8 (0x05);
}

int stack_offset;

void
emit_pop_a ()
{
  // pop %rax
  emit_8 (0x58);
  stack_offset -= 1;
}

void
emit_pop_rdi ()
{
  // pop %rdi
  emit_8 (0x5f);
  stack_offset -= 1;
}

void
emit_pop_rsi ()
{
  // pop %rsi
  emit_8 (0x5e);
  stack_offset -= 1;
}

void
emit_pop_rdx ()
{
  // pop %rdx
  emit_8 (0x5a);
  stack_offset -= 1;
}

void
emit_pop_r10 ()
{
  // pop %r10
  emit_8 (0x41);
  emit_8 (0x5a);
  stack_offset -= 1;
}

void
emit_pop_r8 ()
{
  // pop %r8
  emit_8 (0x41);
  emit_8 (0x58);
  stack_offset -= 1;
}

void
emit_pop_r9 ()
{
  // pop %r9
  emit_8 (0x41);
  emit_8 (0x59);
  stack_offset -= 1;
}

void
emit_pops (int n)
{
  if (n != 0)
    {
      // add $8*N, %rsp
      emit_8 (0x48);
      emit_8 (0x81);
      emit_8 (0xC4);
      emit_32 (8*n);
      stack_offset -= n;
    }
}

void
emit_push_a ()
{
  // push %rax
  emit_8 (0x50);
  stack_offset += 1;
}

void
emit_fetch_a (int o)
{
  // mov 8*o(%rsp), %rax
  emit_8 (0x48);
  emit_8 (0x8B);
  emit_8 (0x84);
  emit_8 (0x24);
  emit_32 (8*o);
}

void
emit_store_a (int o)
{
  // mov %rax, 8*o(%rsp)
  emit_8 (0x48);
  emit_8 (0x89);
  emit_8 (0x84);
  emit_8 (0x24);
  emit_32 (8*o);
}

uint64_t
emit_jne_a ()
{
  // test %rax,%rax
  emit_8 (0x48);
  emit_8 (0x85);
  emit_8 (0xC0);

  // jne
  emit_8 (0x0f);
  emit_8 (0x85);
  uint64_t off = code_offset;
  emit_32 (0);
  return off;
}

uint64_t
emit_jmp ()
{
  uint64_t off;
  emit_8 (0xe9);
  off = code_offset;
  emit_32 (0);
  return off;
}

uint64_t
emit_call ()
{
  uint64_t off;
  emit_8 (0xe8);
  off = code_offset;
  emit_32 (0);
  return off;
}

void
emit_call_a ()
{
  emit_8 (0xFF);
  emit_8 (0xD0);
}

void
emit_ret ()
{
  emit_8 (0xc3);
}

void
emit_null ()
{
}

void
emit_pop_b ()
{
  // pop %rbx
  emit_8 (0x5B);
  stack_offset -= 1;
}

void
emit_pop_add ()
{
  emit_pop_b ();

  // add %rbx, %rax
  emit_8 (0x48);
  emit_8 (0x01);
  emit_8 (0xd8);
}

void
emit_pop_sub ()
{
  emit_pop_b ();

  // sub %rbx, %rax
  emit_8 (0x48);
  emit_8 (0x29);
  emit_8 (0xd8);
}

void
emit_neg ()
{
  // neg %rax
  emit_8 (0x48);
  emit_8 (0xf7);
  emit_8 (0xd8);
}

void
emit_not ()
{
  // test %rax,%rax
  emit_8 (0x48);
  emit_8 (0x85);
  emit_8 (0xC0);

  // sete %al
  emit_8 (0x0F);
  emit_8 (0x94);
  emit_8 (0xC0);

  // movzbl %al,%eax
  emit_8 (0x0F);
  emit_8 (0xB6);
  emit_8 (0xC0);
}

void
emit_ref_byte ()
{
  // movsbl (%rax),%eax
  emit_8 (0x0f);
  emit_8 (0xbe);
  emit_8 (0x00);
}

/* Lists */

typedef struct exp {
  enum {
    pair_exp, sym_exp, inum_exp, dnum_exp, singleton_exp
  } type;
  union {
    struct {
      struct exp *first;
      struct exp *rest;
    };
    struct {
      char *sym;
      struct exp *link;
    };
    int64_t inum;
    double dnum;
  };
} exp;

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

exp *
alloc_exp ()
{
  return obstack_alloc (&exp_obs, sizeof(exp));
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
sym (char *sym)
{
  unsigned int hash = 5381;

  char *p = sym;
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
reverse (exp *e)
{
  exp *r = nil ();
  while (is_pair (e))
    {
      exp *n = e->rest;
      e->rest = r;
      r = e;
      e = n;
    }
  return r;
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
    fprintf (f, "<???>");
}

#define token_size 1024
char token[token_size];
enum { punct_tok, sym_tok, inum_tok, dnum_tok, eof_tok } token_kind;

int lineno = 1;

void
read_token ()
{
  token[0] = '\0';

  int c;
  while (isspace (c = getchar ()))
    {
      if (c == '\n')
        lineno++;
    }
  if (c == '(' || c == ')')
    {
      token[0] = c;
      token[1] = '\0';
      token_kind = punct_tok;
    }
  else if (c == '\'')
    {
      bool escape_next = false;
      int i = 0;
      while ((c = getchar ()) != EOF && c != '\'' && !escape_next)
        {
          if (c == '\n')
            lineno++;
          if (c == '\\' && !escape_next)
            escape_next = true;
          else
            token[i++] = c;
        }
      token[i] = '\0';
      token_kind = sym_tok;
    }
  else if (isdigit (c))
    {
      int i = 0;
      token_kind = inum_tok;
      token[i++] = c;
      while (isdigit (c = getchar ()) || c == '.')
        {
          token[i++] = c;
          if (c == '.')
            token_kind = dnum_tok;
        }
      token[i] = '\0';
      ungetc (c, stdin);
    }
  else if (c != EOF)
    {
      int i = 0;
      token_kind = sym_tok;
      token[i++] = c;
      while ((c = getchar ()) != EOF && !isspace (c) && c != '(' && c != ')')
        token[i++] = c;
      token[i] = '\0';
      ungetc (c, stdin);
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
    return inum (strtol (token, NULL, 10));
  else if (token_kind == dnum_tok)
    return dnum (strtod (token, NULL));
  else if (token_kind == sym_tok)
    return sym (token);
  else if (token_kind == punct_tok && token[0] == '(')
    {
      exp *e = nil ();
      while (true) {
        read_token ();
        if (token_kind == punct_tok && token[0] == ')')
          break;
        e = cons (read_exp_1 (), e);
      }
      return reverse (e);
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

/* Compiler */

void
error (exp *form, const char *msg)
{
  fprintf (stderr, "%d: ", lineno);
  write_exp (stderr, form);
  fprintf (stderr, "\n");
  exitf (1, "error: %s", msg);
}

struct obstack global_decl_obs;
struct obstack local_decl_obs;

typedef struct decl {
  struct decl *next;
  enum {
    global_func,
    local_var
  } kind;
  exp *name;
  int offset;
  bool defined;
  struct funcref *refs;
} decl;

typedef struct funcref {
  struct funcref *next;
  uint64_t offset;
  bool absolute;
} funcref;

typedef struct labdef {
  struct labdef *next;
  struct labref *refs;
  exp *name;
  uint64_t offset;
  bool defined;
} labdef;

typedef struct labref {
  struct labref *next;
  uint64_t offset;
} labref;

decl *global_decls;
decl *local_decls;
labdef *labdefs;

void
init_decls ()
{
  obstack_init (&global_decl_obs);
  global_decls = NULL;

  obstack_init (&local_decl_obs);
  local_decls = NULL;
  labdefs = NULL;
}

void
reset_locals ()
{
  obstack_free (&local_decl_obs, NULL);
  obstack_init (&local_decl_obs);
  local_decls = NULL;
  labdefs = NULL;
  stack_offset = 0;
}

void
declare_local_var (exp *sym, int offset)
{
  if (!is_sym (sym))
    error (sym, "variable name must be symbol");

  decl *d = obstack_alloc (&local_decl_obs, sizeof(decl));
  d->kind = local_var;
  d->name = sym;
  d->offset = offset;
  d->next = local_decls;
  local_decls = d;
}

decl *
find_decl (exp *sym)
{
  for (decl *d = local_decls; d; d = d->next)
    if (d->name == sym)
      return d;

  for (decl *d = global_decls; d; d = d->next)
    if (d->name == sym)
      return d;

  decl *d = obstack_alloc (&global_decl_obs, sizeof (decl));
  d->kind = global_func;
  d->name = sym;
  d->defined = false;
  d->next = global_decls;
  global_decls = d;
  return d;
}

labdef *
get_labdef (exp *sym)
{
  if (!is_sym (sym))
    error (sym, "label name must be symbol");

  for (labdef *l = labdefs; l; l = l->next)
    if (l->name == sym)
      return l;

  labdef *l = obstack_alloc (&local_decl_obs, sizeof(labdef));
  l->name = sym;
  l->defined = false;
  l->next = labdefs;
  labdefs = l;
}

void
define_global_func (exp *sym)
{
  decl *d = find_decl (sym);
  if (d->kind != global_func)
    error (sym, "not a function");
  if (d->defined)
    error (sym, "redefined");
  d->offset = code_offset;
  d->defined = true;
}

void
reference_global_func (decl *d, uint64_t offset)
{
  funcref *r = obstack_alloc (&global_decl_obs, sizeof(funcref));
  r->offset = offset;
  r->absolute = false;
  r->next = d->refs;
  d->refs = r;
}

void
reference_global_func_abs (decl *d, uint64_t offset)
{
  funcref *r = obstack_alloc (&global_decl_obs, sizeof(funcref));
  r->offset = offset;
  r->absolute = true;
  r->next = d->refs;
  d->refs = r;
}

void
resolve_global_funcs ()
{
  for (decl *d = global_decls; d; d = d->next)
    {
      if (d->kind != global_func)
        continue;
      if (!d->defined)
        exitf (1, "function '%s' is not defined", sym_name (d->name));
      symtab_add (sym_name (d->name), d->offset);
      for (funcref *r = d->refs; r; r = r->next)
        {
          if (r->absolute)
            *(uint32_t *)(code + r->offset) = code_start + d->offset;
          else
            *(uint32_t *)(code + r->offset) = d->offset - r->offset - 4;
        }
    }
}

void
define_local_lab (exp *sym)
{
  labdef *l = get_labdef (sym);
  if (l->defined)
    error (sym, "redefined");
  l->offset = code_offset;
  l->defined = true;
}

void
reference_local_lab (exp *sym, uint64_t offset)
{
  labdef *l = get_labdef (sym);
  labref *r = obstack_alloc (&local_decl_obs, sizeof(labref));
  r->offset = offset;
  r->next = l->refs;
  l->refs = r;
}

void
resolve_local_labs ()
{
  for (labdef *l = labdefs; l; l = l->next)
    {
      if (!l->defined)
        exitf (1, "label %s is not defined", sym_name (l->name));
      for (labref *r = l->refs; r; r = r->next)
        *(uint32_t *)(code + r->offset) = l->offset - r->offset - 4;
    }
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

// Compiling an expression leaves the result in %rax.

void compile_exp (exp *e);

void
compile_exps (exp *e)
{
  if (is_pair (rest (e)))
    {
      compile_exps (rest (e));
      emit_push_a ();
    }
  compile_exp (first (e));
}

void
compile_call (exp *e)
{
  compile_exps (e);
  emit_call_a ();
  emit_pops (len(e)-1);
}

void
compile_syscall (exp *e)
{
  int l = len (rest (e));
  if (l < 1 || l > 7)
    error (e, "only 1 to 7 arguments to syscall");

  compile_exps (rest (e));
  if (l > 1) emit_pop_rdi ();
  if (l > 2) emit_pop_rsi ();
  if (l > 3) emit_pop_rdx ();
  if (l > 4) emit_pop_r10 ();
  if (l > 5) emit_pop_r8 ();
  if (l > 6) emit_pop_r9 ();
  emit_syscall ();
}

void
compile_multi_op (exp *e, void (*emit_op)(void), void (*emit_single)(void))
{
  int l = len (e);

  if (l == 1)
    error (e, "syntax");
  else if (l == 2)
    {
      compile_exp (second (e));
      emit_single ();
    }
  else
    {
      compile_exps (rest (e));
      while (l > 2)
        {
          emit_op ();
          l -= 1;
        }
    }
}

void
compile_single_op (exp *e, void (*emit_op)(void))
{
  exp *val;
  parse_form (e, 1, 0, &val);
  compile_exp (val);
  emit_op ();
}

void
compile_exp (exp *e)
{
  if (is_inum (e))
    emit_set (inum_val (e));
  else if (is_sym (e))
    {
      decl *d = find_decl (e);
      if (d->kind == local_var)
        emit_fetch_a (stack_offset - d->offset);
      else if (d->kind == global_func)
        {
          uint64_t offset = emit_set (0);
          reference_global_func_abs (d, offset);
        }
      else
        error (e, "internal error");
    }
  else if (is_form (e, "+"))
    compile_multi_op (e, emit_pop_add, emit_null);
  else if (is_form (e, "-"))
    compile_multi_op (e, emit_pop_sub, emit_neg);
  else if (is_form (e, "not"))
    compile_single_op (e, emit_not);
  else if (is_form (e, "b@"))
    {
      exp *ptr, *off;
      parse_form (e, 1, 1, &ptr, &off);
      if (off)
        {
          compile_exp (off);
          emit_push_a ();
          compile_exp (ptr);
          emit_pop_add ();
          emit_ref_byte ();
        }
      else
        {
          compile_exp (ptr);
          emit_ref_byte ();
        }
    }
  else if (is_form (e, "syscall"))
    compile_syscall (e);
  else if (is_pair (e))
    compile_call (e);
  else
    error (e, "syntax");
}

bool
compile_statement (exp *e)
{
  exp *label, *cond;

  if (is_sym (e))
    {
      define_local_lab (e);
    }
  else if (is_form (e, "="))
    {
      exp *var, *val;
      parse_form (e, 2, 0, &var, &val);
      decl *d = find_decl (var);
      if (d->kind != local_var)
        error (e, "can only assign to variables");
      compile_exp (val);
      emit_store_a (stack_offset - d->offset);
    }
  else if (is_form (e, "goto"))
    {
      exp *label, *cond;
      uint64_t offset;
      parse_form (e, 1, 1, &label, &cond);
      if (cond)
        {
          compile_exp (cond);
          offset = emit_jne_a ();
        }
      else
        offset = emit_jmp ();
      reference_local_lab (label, offset);
    }
  else if (is_form (e, "return"))
    {
      exp *val;
      parse_form (e, 0, 1, &val);
      if (val)
        compile_exp (val);
      else
        emit_set (0);
      emit_pops (stack_offset);
      emit_ret ();
      return true;
    }
  else
    compile_exp (e);

  return false;
}

void
compile_function (exp *e)
{
  if (len (e) < 2)
    error (e, "syntax error");

  exp *head = second (e);
  exp *body = rest (rest (e));

  if (len (head) < 1)
    error (e, "syntax error");

  reset_locals ();

  define_global_func (first (head));
  int off = -1;
  for (exp *params = rest (head); is_pair (params); params = rest (params))
    {
      declare_local_var (first (params), off);
      off -= 1;
    }

  bool in_decls = true;
  bool did_return = false;
  while (is_pair (body))
    {
      exp *s = first (body);
      if (is_form (s, "var"))
        {
          if (!in_decls)
            error (s, "variable declarations must be at start of body");
          if (len (s) == 3)
            compile_exp (third (s));
          else if (len (s) == 2)
            emit_set (0);
          else
            error (s, "variable declarations must have 1 or 2 arguments");
          emit_push_a ();
          declare_local_var (second (s), stack_offset);
        }
      else
        {
          in_decls = false;
          did_return = compile_statement (s);
        }
      body = rest (body);
    }

  if (!did_return)
    {
      emit_set (0);
      emit_ret ();
    }

  resolve_local_labs ();
}

void
compile_data (exp *e)
{
  if (len (e) < 2)
    error (e, "syntax");

  exp *sym = second (e);

  define_global_func (sym);

  e = rest (rest (e));
  while (is_pair (e))
    {
      if (is_form (first (e), "b"))
        {
          exp *b = rest (first (e));
          while (is_pair (b))
            {
              if (is_sym (first (b)))
                {
                  for (const char *bytes = sym_name (first (b)); *bytes; bytes++)
                    emit_8 (*bytes);
                }
              else if (is_inum (first (b)))
                emit_8 (inum_val (first (b)));
              else
                error (b, "syntax");
              b = rest (b);
            }
        }
      else
        error (e, "sorry");
      e = rest (e);
    }
}

void
compile_global (exp *e)
{
  if (is_form (e, "fun"))
    compile_function (e);
  else if (is_form (e, "data"))
    compile_data (e);
  else
    error (e, "syntax error");
}

void
compile_start ()
{
  exp *e = cons (sym ("syscall"), cons (inum (60), cons (cons (sym ("main"), nil ()), nil ())));
  symtab_add ("start", code_offset);
  compile_exp (e);
}

/* Main */

void
main ()
{
  init_exp();
  init_code ();
  init_decls ();

  compile_start ();

  exp *e;
  while (!is_end_of_file (e = read_exp ()))
    compile_global (e);

  resolve_global_funcs ();

  dump_code ();
  exit (0);
}
