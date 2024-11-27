/* $Id: gen_code.h,v 1.7 2023/11/14 04:41:00 leavens Exp $ */
#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code.h"
#include "code_seq.h"

// Initialize the code generator
extern void gen_code_initialize();

// Requires: bf if open for writing in binary
// Generate code for the given AST
extern void gen_code_program(BOFFILE bf, block_t prog);

// Requires: bf if open for writing in binary
// Generate code for prog into bf
extern void gen_code_program(BOFFILE bf, block_t prog);

// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_var_decls(var_decls_t vds);

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_var_decl(var_decl_t vd);

//-----------------------CONSTANTS--------------------------------------
// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_const_decls(const_decls_t cds);

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_const_decl(const_decl_t cd);

// Generate code for the identififers in idents with type t
// in reverse order (so the first declared are allocated last).
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
extern code_seq gen_code_idents(ident_list_t idents);

// Generate code for stmt
extern code_seq gen_code_stmt(stmt_t stmt);

// // Generate code for stmt
// extern code_seq gen_code_begin_stmt(begin_stmt_t stmt);

// Generate code for the list of statments given by stmts to out
extern code_seq gen_code_stmts(stmts_t stmts);

// Generate code for stmt
extern code_seq gen_code_assign_stmt(assign_stmt_t stmt);

// Generate code for stmt
extern code_seq gen_code_call_stmt(call_stmt_t stmt);

// Generate code for the if-statment given by stmt
extern code_seq gen_code_if_stmt(if_stmt_t stmt);

// Generate code for the write statment given by stmt.
extern code_seq gen_code_while_stmt(while_stmt_t stmt);

// Generate code for the read statment given by stmt
extern code_seq gen_code_read_stmt(read_stmt_t stmt);

// Generate code for the read statment given by stmt
extern code_seq gen_code_print_stmt(print_stmt_t stmt);

// Generate code for the read statment given by stmt
extern code_seq gen_code_block_stmt(block_stmt_t stmt);

// // Generate code for the write statment given by stmt.
// extern code_seq gen_code_write_stmt(write_stmt_t stmt);

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_expr(expr_t exp);

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

// Generate code to apply op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_op(token_t op, type_exp_e typ);

// Generate code to apply the floating-point arith_op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
extern code_seq gen_code_arith_op(token_t arith_op);

// Generate code for the rel_op
// applied to 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// Also modifies SP when executed
extern code_seq gen_code_rel_op(token_t rel_op, type_exp_e typ);

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
extern code_seq gen_code_ident(ident_t id);

// Generate code to put the given number on top of the stack
// Modifies V0 when executed
extern code_seq gen_code_number(number_t num);

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
extern code_seq gen_code_logical_not_expr(expr_t exp);

#define MINIMAL_STACK_ALLOC_BYTES (BYTES_PER_WORD*MINIMAL_STACK_ALLOC_IN_WORDS)

extern code_seq code_save_registers_for_AR();

extern code_seq code_restore_registers_from_AR();

// Requires: bytes > 0 && (bytes % BYTES_PER_WORD) == 0
// Deallocate the given number of bytes on the runtime stack
// Modifies when executed: SP register
extern code_seq code_deallocate_stack_space(immediate_type bytes);

code *code_exit();
code_seq code_pop_stack_into_reg(reg_num_type reg);

// Requires: reg != FP
// Modifies only: register reg
// Return a code sequence that will put the address that corresponds to the
// frame pointer for the given number of scopes outward in register reg
code_seq code_compute_fp(reg_num_type reg, unsigned int levelsOut);

code_seq code_load_static_link(reg_num_type rt, reg_num_type rb);
// Offset from the FP where the static link is found in an AR
#define STATIC_LINK_OFFSET (-3)
#endif