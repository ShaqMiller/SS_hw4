/* $Id: gen_code.h,v 1.7 2023/11/14 04:41:00 leavens Exp $ */
#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code.h"
#include "code_seq.h"

static void gen_code_output_program(BOFFILE bf, code_seq main_cs);
static BOFHeader gen_code_program_header(code_seq main_cs);
static void gen_code_output_seq(BOFFILE bf, code_seq cs);
static void gen_code_output_literals(BOFFILE bf);
// Initialize the code generator
extern void gen_code_initialize();

// Requires: bf if open for writing in binary
// Generate code for the given AST
extern void gen_code_program(BOFFILE bf, block_t prog);


// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_var_decls(var_decls_t vds);

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_var_decl(var_decl_t vd);



// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_const_decls(const_decls_t vds);

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_const_decl(const_decl_t vd);


// Generate code for the identififers in idents with type t
// in reverse order (so the first declared are allocated last).
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_idents(ident_list_t idents);

// Generate code for the list of statments given by stmts to out
extern code_seq gen_code_stmts(stmts_t stmts);

// Generate code for stmt
extern code_seq gen_code_stmt(stmt_t stmt);


//STATEMENT--------------------------
extern code_seq gen_code_print_stmt(print_stmt_t stmt); 


//EXPRESSIONS-----------------------------
// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_expr(expr_t exp);

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
extern code_seq gen_code_ident(ident_t id);

// Generate code to put the given number on top of the stack
// Modifies V0 when executed
extern code_seq gen_code_number(number_t num);

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
extern code_seq gen_code_negated_expr(negated_expr_t exp);


// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

#endif