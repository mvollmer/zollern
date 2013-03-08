/*
   Z0 -- Bootstrap language.

   The first language is very primitive: there are no types, no memory
   management, and only primitive control structures.  It is compiled
   to machine code by this program.

   This language is only used to write one program: A compiler for a
   better language.  As such, it doesn't have much in the way of
   convenience or performance.

 # Global definitions

   - (fun (SYMBOL SYMBOL...)
       LOCAL-DEFINITION...
       STATEMENT...)

   Function definition.  The first symbol is the name of the function,
   the rest are the names for the arguments.

   - (data SYMBOL (o EXPR...) (t EXPR...) (w EXPR...) (b EXPR...) ...)

   Data definition.

 # Local definitions

   - (var SYMBOL EXPR)
   - (var SYMBOL)

   Variable definition.

 # Statements

   - SYMBOL

   A label.  (Note that labels can not appear between definitions.)

   - (goto SYMBOL EXPR)

   Transfer control to the named label if the given expression is
   non-zero.

   - (return EXPR)

   Return the value of EXPR from the current function.  If the end of
   a functions body is reached, the return value is 0.

   - (= SYMBOL EXPR)

   Assignment.  SYMBOL must refer to a variable.

   - (o@ EXPR EXPR)
   - (t@ EXPR EXPR)
   - (w@ EXPR EXPR)
   - (b@ EXPR EXPR)

   Store a octa, tetra, wyde, or byte.  First expression is the
   address, the second is the value.

 # Expressions

   - NUMBER

   A literal.

   - SYMBOL

   Variable, constant, function, or argument reference.  For a
   function or data definition, the value is the address of the object
   in memory.

   - (if EXPR EXPR EXPR)

   Conditional expression.

   - (and EXPR...)
   - (or EXPR...)

   Short circuit logical operators.  The value of the whole expression
   is the value of the last EXPR that has been evaluated.

   - (OP EXPR...)

   Arithmetic on signed integers, where OP is one of +, -, * /, %.

   - (dOP EXPR...)

   Arithmetic on double precision floats.

   - (uOP EXPR...)

   Arithmetic on unsigned integers.

   - (& SYMBOL)

   Address of SYMBOL, which must refer to a variable.

   - (o@ EXPR)
   - (t@ EXPR)
   - (w@ EXPR)
   - (b@ EXPR)

   Read the octa, tetra, wyde, or byte at the given address as a
   signed value.

   - (uo@ EXPR)
   - (ut@ EXPR)
   - (uw@ EXPR)
   - (ub@ EXPR)

   Read the octa, tetra, wyde, or byte at the given address as a
   unsigned value.

   - (syscall ...)

   Call into the kernel.

   - (EXPR EXPR...)

   Function call.  The first expression is the address of the function
   to call, the rest are the arguments.

 # Run time environment

 There really isn't much.

 The state of a program is a program counter and a stack pointer.  The
 stack grows down and is used by the compiler for function calls,
 local variables and also when evaluating expressions.

 A program is started by calling a function called main without any
 arguments.  It can use memory from address 0 to 1 GiB.
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

void
init_code()
{
  code = xmalloc (1024*1024);
  code_ptr = code;
  code_end = code + 1024*1024;
  code_offset = 0;
}

void
dump_code()
{
  uint64_t code_size = code_offset;

  struct file {
    Elf64_Ehdr h;
    Elf64_Phdr ph[2];
    Elf64_Shdr sh[4];
    Elf64_Sym symtab[3];
    char strtab[5*16];
  } f;

  uint64_t vaddr = 0x40000000;

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

  f.sh[1].sh_name = 1*16;
  f.sh[1].sh_type = SHT_PROGBITS;
  f.sh[1].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
  f.sh[1].sh_addr = vaddr + sizeof (f);
  f.sh[1].sh_offset = sizeof (f);
  f.sh[1].sh_size = code_size;
  f.sh[1].sh_link = 0;
  f.sh[1].sh_info = 0;
  f.sh[1].sh_addralign = 4;
  f.sh[1].sh_entsize = 0;

  f.sh[2].sh_name = 2*16;
  f.sh[2].sh_type = SHT_STRTAB;
  f.sh[2].sh_flags = 0;
  f.sh[2].sh_addr = 0;
  f.sh[2].sh_offset = offsetof (struct file, strtab);
  f.sh[2].sh_size = sizeof (f.strtab);
  f.sh[2].sh_link = 0;
  f.sh[2].sh_info = 0;
  f.sh[2].sh_addralign = 1;
  f.sh[2].sh_entsize = 0;

  f.sh[3].sh_name = 3*16;
  f.sh[3].sh_type = SHT_SYMTAB;
  f.sh[3].sh_flags = 0;
  f.sh[3].sh_addr = 0;
  f.sh[3].sh_offset = offsetof (struct file, symtab);
  f.sh[3].sh_size = sizeof (f.symtab);
  f.sh[3].sh_link = 2;
  f.sh[3].sh_info = 2;
  f.sh[3].sh_addralign = 8;
  f.sh[3].sh_entsize = sizeof (f.symtab[0]);

  f.symtab[0].st_name = 0;
  f.symtab[0].st_info = 0;
  f.symtab[0].st_other = 0;
  f.symtab[0].st_shndx = 0;
  f.symtab[0].st_value = 0;
  f.symtab[0].st_size = 0;

  f.symtab[1].st_name = 0;
  f.symtab[1].st_info = ELF64_ST_INFO (STB_LOCAL, STT_SECTION);
  f.symtab[1].st_other = STV_DEFAULT;
  f.symtab[1].st_shndx = 1;
  f.symtab[1].st_value = vaddr + sizeof(f);
  f.symtab[1].st_size = 0;

  f.symtab[2].st_name = 4*16;
  f.symtab[2].st_info = ELF64_ST_INFO (STB_GLOBAL, STT_NOTYPE);
  f.symtab[2].st_other = STV_DEFAULT;
  f.symtab[2].st_shndx = 1;
  f.symtab[2].st_value = vaddr + sizeof(f);
  f.symtab[2].st_size = 0;

  strcpy (f.strtab, "");
  strcpy (f.strtab+1*16, ".text");
  strcpy (f.strtab+2*16, ".strtab");
  strcpy (f.strtab+3*16, ".symtab");
  strcpy (f.strtab+4*16, "_start");

  if (write (1, &f, sizeof(f)) != sizeof(f)
      || write (1, code, code_size) != code_size)
    exitf(1, "Can't write code: %m");

  free (code);
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

void
emit_mov_rdi (uint32_t t)
{
  emit_8 (0x48);
  emit_8 (0xC7);
  emit_8 (0xC7);
  emit_32 (t);
}

void
emit_mov_rax (uint32_t t)
{
  emit_8 (0x48);
  emit_8 (0xC7);
  emit_8 (0xC0);
  emit_32 (t);
}

void
emit_mov_a_rdi ()
{
  emit_8 (0x48);
  emit_8 (0x89);
  emit_8 (0xC7);
}

void
emit_syscall ()
{
  emit_8 (0x0F);
  emit_8 (0x05);
}

int stack_offset;

void
emit_push (uint64_t val)
{
  // pushq LO
  emit_8 (0x68);
  emit_32 (val & 0xFFFFFFFF);
  if (val >> 32)
    {
      // movl HI,4(%rsp)
      emit_8 (0xc7);
      emit_8 (0x44);
      emit_8 (0x24);
      emit_8 (0x04);
      emit_32 (val >> 32);
    }

  stack_offset += 1;
}

void
emit_pop_a ()
{
  // pop %rax
  emit_8 (0x58);
  stack_offset -= 1;
}

void
emit_pop_b ()
{
  // pop %rbx
  emit_8 (0x5B);
  stack_offset -= 1;
}

void
emit_pops (int n)
{
  // add $8*N, %rsp
  emit_8 (0x48);
  emit_8 (0x81);
  emit_8 (0xC4);
  emit_32 (8*n);
  stack_offset -= n;
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

uint64_t
emit_jne_a ()
{
  uint64_t off;
  emit_8 (0x0f);
  emit_8 (0x85);
  off = code_offset;
  emit_32 (0);
  return off;
}

void
emit_null ()
{
}

void
emit_add ()
{
  emit_pop_a ();
  emit_pop_b ();

  // add %rbx, %rax
  emit_8 (0x48);
  emit_8 (0x01);
  emit_8 (0xd8);

  emit_push_a ();
}

void
emit_sub ()
{
  emit_pop_b ();
  emit_pop_a ();

  // sub %rbx, %rax
  emit_8 (0x48);
  emit_8 (0x29);
  emit_8 (0xd8);

  emit_push_a ();
}

void
emit_neg ()
{
  emit_pop_a ();

  // neg %rax
  emit_8 (0x48);
  emit_8 (0xf7);
  emit_8 (0xd8);

  emit_push_a ();
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

struct obstack local_obs;

typedef struct local_var {
  struct local_var *next;
  exp *name;
  int offset;
} local_var;

typedef struct local_labdef {
  struct local_labdef *next;
  struct local_labref *refs;
  exp *name;
  uint64_t offset;
  bool defined;
} local_labdef;

typedef struct local_labref {
  struct local_labref *next;
  uint64_t offset;
} local_labref;

local_var *local_vars;
local_labdef *local_labs;

void
init_locals ()
{
  obstack_init (&local_obs);
  local_vars = NULL;
  local_labs = NULL;
}

void
reset_locals ()
{
  obstack_free (&local_obs, NULL);
  init_locals ();
}

void
declare_local_var (exp *sym)
{
  if (!is_sym (sym))
    error (sym, "variable name must be symbol");

  local_var *l = obstack_alloc (&local_obs, sizeof(local_var));
  l->name = sym;
  l->offset = stack_offset;
  l->next = local_vars;
  local_vars = l;
}

local_var *
find_local_var (exp *sym)
{
  for (local_var *l = local_vars; l; l = l->next)
    if (l->name == sym)
      return l;
  return NULL;
}

local_labdef *
get_local_lab (exp *sym)
{
  if (!is_sym (sym))
    error (sym, "label name must be symbol");

  for (local_labdef *l = local_labs; l; l = l->next)
    if (l->name == sym)
      return l;

  local_labdef *l = obstack_alloc (&local_obs, sizeof(local_labdef));
  l->name = sym;
  l->defined = false;
  l->next = local_labs;
  local_labs = l;
}

void
define_local_lab (exp *sym)
{
  local_labdef *l = get_local_lab (sym);
  l->offset = code_offset;
  l->defined = true;
}

void
reference_local_lab (exp *sym, uint64_t offset)
{
  local_labdef *l = get_local_lab (sym);
  local_labref *r = obstack_alloc (&local_obs, sizeof(local_labref));
  r->offset = offset;
  r->next = l->refs;
  l->refs = r;
}

void
resolve_local_labs ()
{
  for (local_labdef *l = local_labs; l; l = l->next)
    {
      if (!l->defined)
        exitf (1, "label %s is not defined", sym_name (l->name));
      for (local_labref *r = l->refs; r; r = r->next)
        *(uint32_t *)(code + r->offset) = l->offset - r->offset - 4;
    }
}

bool
is_form (exp *e, const char *sym)
{
  return (is_pair (e) &&
          is_sym (first (e)) && strcmp (sym_name (first (e)), sym) == 0);
}

void compile_exp (exp *e);

void
compile_list (exp *e, void (*emit_op) (), void (*emit_single) ())
{
  if (is_nil (e))
    error (e, "arguments can't be empty");
  compile_exp (first (e));
  exp *r = rest (e);
  if (is_nil (r))
    emit_single ();
  else
    {
      while (!is_nil (r))
        {
          compile_exp (first (r));
          emit_op ();
          r = rest (r);
        }
    }
}

void
compile_exp (exp *e)
{
  if (is_inum (e))
    emit_push (inum_val (e));
  else if (is_sym (e))
    {
      local_var *l = find_local_var (e);
      if (l == NULL)
        error (e, "undefined variable");
      emit_fetch_a (stack_offset - l->offset);
      emit_push_a ();
    }
  else if (is_form (e, "+"))
    compile_list (rest (e), emit_add, emit_null);
  else if (is_form (e, "-"))
    compile_list (rest (e), emit_sub, emit_neg);
  else
    error (e, "syntax");
}

void
compile_return ()
{
  emit_pop_a ();
  emit_pops (stack_offset);

  emit_mov_a_rdi ();
  emit_mov_rax (60);
  emit_syscall ();
}

void
compile_statement (exp *e)
{
  if (is_sym (e))
    {
      define_local_lab (e);
    }
  else if (is_form (e, "goto"))
    {
      if (len (e) != 3)
        error (e, "goto needs two arguments");
      exp *lab = first (rest (e));
      exp *cond = first (rest (rest (e)));
      compile_exp (cond);
      emit_pop_a ();
      uint64_t offset = emit_jne_a ();
      reference_local_lab (lab, offset);
    }
  else if (is_form (e, "return"))
    {
      if (len (e) == 2)
        compile_exp (first (rest (e)));
      else if (len (e) == 1)
        emit_push (0);
      else
        error (e, "return needs 0 or 1 argument");

      emit_pop_a ();
      emit_pops (stack_offset);

      emit_mov_a_rdi ();
      emit_mov_rax (60);
      emit_syscall ();
    }
  else
    error (e, "syntax");
}

void
compile_body (exp *e)
{
  reset_locals ();

  bool in_decls = true;
  while (is_pair (e))
    {
      exp *s = first (e);
      if (is_form (s, "var"))
        {
          if (!in_decls)
            error (s, "variable declarations must be at start of body");
          if (len (s) == 3)
            compile_exp (first (rest (rest (s))));
          else if (len (s) == 2)
            emit_push (0);
          else
            error (s, "variable declarations must have 1 or 2 arguments");
          declare_local_var (first (rest (s)));
        }
      else
        {
          in_decls = false;
          compile_statement (s);
        }
      e = rest (e);
    }
  emit_push (0);
  compile_return ();

  resolve_local_labs ();
}

/* Main */

void
main ()
{
  init_exp();
  init_code ();
  init_locals ();

  exp *body = nil ();
  exp *e;
  while (!is_end_of_file (e = read_exp ()))
    body = cons (e, body);

  compile_body (reverse (body));

  dump_code ();
  exit (0);
}
