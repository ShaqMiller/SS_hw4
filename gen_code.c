/* $Id: gen_code.c,v 1.25 2023/11/28 22:12:58 leavens Exp $ */
#include <limits.h>
#include <string.h>
#include "spl.tab.h"
#include "ast.h"
#include "code.h"
#include "id_use.h"
#include "literal_table.h"
#include "gen_code.h"
#include "utilities.h"
#include "regname.h"
#include "code_seq.h"
#include "stdlib.h"
#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();
}


void gen_code_program(BOFFILE bf, block_t prog){

    code_seq main_cs;
    // We want to make the main program's AR look like all blocks... so:
    // allocate space and initialize any variables
    main_cs = gen_code_var_decls(prog.var_decls);
    int vars_len_in_bytes = (code_seq_size(main_cs) / 2) * BYTES_PER_WORD;


    // Handle constant variables
    // code_seq const_cs = gen_code_const_decls(prog.var_decls);
    // code_seq_concat(&main_cs, const_cs);


    // // there is no static link for the program as a whole,
    // // so nothing to do for saving FP into A0 as would be done for a block
    // main_cs = code_seq_concat(main_cs, code_save_registers_for_AR());
    code_seq_concat(&main_cs, gen_code_stmts(prog.stmts));

    //code_seq_concat(&main_cs, code_restore_registers_from_AR());
    //code_seq_concat(&main_cs, code_deallocate_stack_space(vars_len_in_bytes));
    code_seq_add_to_end(&main_cs, code_exit(0));
    gen_code_output_program(bf, main_cs);
}

// Requires: bf is open for writing in binary
// Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}
static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
        word_type w = literal_table_iteration_next();
        // debug_print("Writing literal %f to BOF file\n", w);
        bof_write_word(bf, w);
    }
    literal_table_end_iteration(); // not necessary
}

// Requires: bf if open for writing in binary
// and prior to this scope checking and type checking have been done.
// Write all the instructions in cs to bf in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs)) {
	bin_instr_t inst = code_seq_first(cs)->instr;
	instruction_write_bin_instr(bf, inst);
	cs = code_seq_rest(cs);
    }
}

// Return a header appropriate for the given code
static BOFHeader gen_code_program_header(code_seq main_cs)
{
    BOFHeader ret;
    
    // Set the magic number to "BO32"
    strncpy(ret.magic, "BO32", MAGIC_BUFFER_SIZE);  // "BO32" in ASCII (MAGIC_BUFFER_SIZE is 4)
    
    // Set the start address of the text section (Program Counter address)
    ret.text_start_address = 0;
    
    // Calculate the length of the text section in bytes (words * bytes per word)
    //printf("num inst = %d",code_seq_size(main_cs));
    // ret.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;
    ret.text_length = code_seq_size(main_cs);
    
    // Data section start address should be after the text section and aligned
    // The start address of the data section should be at least after the text section
    int dsa = ret.text_length + BYTES_PER_WORD;  // data start address
    ret.data_start_address = dsa;
    
    // Data length in bytes, assuming literals represent static data
    ret.data_length = literal_table_size() * BYTES_PER_WORD;
    
    // Stack section should start after data section and have space for the stack
    // Stack bottom address should be strictly after data section and should account for the stack space
    int sba = ret.data_start_address + ret.data_length + STACK_SPACE;
    ret.stack_bottom_addr = sba;

    return ret;
}


code_seq gen_code_var_decls(var_decls_t vds){
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while (vdp != NULL) {
        // generate these in reverse order,
        // so the addressing offsets work properly
        code_seq code = gen_code_var_decl(*vdp);
        code_seq_concat(&ret,code);
        vdp = vdp->next;
    }
    return ret;
}



code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_idents(vd.ident_list);
}


// code_seq gen_code_const_decls(const_decls_t cds) {
//     code_seq ret = code_seq_empty();
//     var_decl_t *cdp = cds.start;

//     while (cdp != NULL) {
//         literal_t value = cdp; // Assume `value` holds the constant's value
//         code_seq const_init = code_seq_singleton(code_lit(0, 0, value));
//         code_seq_add_to_end(&const_init, code_swr(SP, 0, 0)); // Store into memory
//         code_seq_concat(&ret, const_init);
        
//         vdp = vdp->next;
//     }
//     return ret;
// }



code_seq gen_code_idents(ident_list_t idents)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = idents.start;

    // We generate code for each identifier and concatenate in reverse order
    while (idp != NULL) {
        // Allocate space for the identifier and set up its value
        code_seq alloc_and_init = code_seq_singleton(code_addi(SP, SP, -BYTES_PER_WORD));
        // Uncomment if the identifier needs to be stored in memory
        // code_seq_add_to_end(&alloc_and_init, code_fsw(SP, 0, 0));
        code_seq_add_to_end(&alloc_and_init, code_swr(SP, 0, SP));

        // Concatenate in reverse order (so it addresses correctly)
        code_seq_concat(&alloc_and_init, ret);
        ret = alloc_and_init;

        idp = idp->next;
    }

    return ret;
}

code_seq gen_code_ident(ident_t ident) {
    code_seq seq = code_seq_empty();
    
    // Assuming ident contains some form of address or reference to its value, 
    // for this example, we'll assume that it's loaded directly from the stack (SP).

    // If the identifier is stored in memory and we need to load its value:
    // Use code_lwi to load the identifier's value from a known memory location into A0.
    // (Here, the memory address or calculation might be different based on your setup)
    
    code_seq_concat(&seq, code_seq_singleton(code_lwi(A0, SP, 0, 0))); // Example load into A0

    // You may need to adjust the stack pointer or do additional operations based on your logic.
    code_seq_concat(&seq, code_seq_singleton(code_ari(SP, 1))); // Adjust stack pointer if needed

    return seq;
}




// Generate code for the list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts)
{
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmt_list.start;
    while (sp != NULL) {
        code_seq_concat(&ret, gen_code_stmt(*sp));
        sp = sp->next;
    }
    return ret;
}

// Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind) {
        case print_stmt:
            return gen_code_print_stmt(stmt.data.print_stmt);
        break;
        case call_stmt:
            break;
        break;
        case if_stmt:
            break;
        break;
        case while_stmt:
            break;
        break;
        case read_stmt:
            break;
        break;
        case block_stmt:
            break;
        break;
        default:
        bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
        break;
    }
    // The following can never execute, but this quiets gcc's warning
    return code_seq_empty();
}

code_seq gen_code_print_stmt(print_stmt_t stmt) {
    code_seq ret = code_seq_empty();
    
    // Generate code to evaluate the expression to print
    code_seq expr_cs = gen_code_expr(stmt.expr);

    // Concatenate the expression's code
    code_seq_concat(&ret, expr_cs);

    // Move the value to print into the correct register
    // Assuming the value to be printed is on the stack and we want to load it into a register
    // Use code_lwi (load word immediate) to load the value from the stack (assuming $sp points to it)
    code_seq_add_to_end(&ret, code_lwi(A0, 0, SP, 0)); // Load the top of the stack into A0 (for printing)

    // Clean up the stack (pop the evaluated value)
    code_seq_add_to_end(&ret, code_addi(SP, SP, BYTES_PER_WORD)); // Move stack pointer to clean up

    // Add the syscall instruction for printing (using code_pint to print an integer)
    code_seq_add_to_end(&ret, code_pint(A0, 0)); // Print the value in A0 (assuming the value is an integer)

    return ret;
}


// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_expr(expr_t exp)
{
    switch (exp.expr_kind) {
        case expr_bin:
            return gen_code_binary_op_expr(exp.data.binary);
        break;
        case expr_ident:
            return gen_code_ident(exp.data.ident);
        break;
        case expr_number:
            return gen_code_number(exp.data.number);
        break;
        case expr_negated:
            return gen_code_negated_expr(exp.data.negated);
        break;
        default:
        bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr",
                exp.expr_kind);
        break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
}

code_seq gen_code_number(number_t num) {
    code_seq seq = code_seq_empty();
    
    // Assuming the value is an integer, push the correct portion onto the stack.
    // For a 32-bit value, you might only need to use the lower 32 bits.
    code_seq_concat(&seq, code_seq_singleton(code_lit(SP, -BYTES_PER_WORD, num.value)));  // Push num.value onto stack
    
    // Adjust the stack pointer
    code_seq_concat(&seq, code_seq_singleton(code_ari(SP, -1)));  // Move SP down by 1 word

    return seq;
}

code_seq gen_code_negated_expr(negated_expr_t exp) {
    code_seq seq = code_seq_empty();
    
    // Generate code for the expression
    code_seq_concat(&seq, gen_code_expr(*exp.expr));

    // Negate the top of the stack
    code_seq_concat(&seq, code_seq_singleton(code_neg(SP, 0, SP, 0)));

    return seq;
}

code_seq gen_code_binary_op_expr(binary_op_expr_t exp) {
    code_seq seq = code_seq_empty();
    
    // Generate code for left and right operands
    code_seq_concat(&seq, gen_code_expr(*exp.expr1));
    code_seq_concat(&seq, gen_code_expr(*exp.expr2));

    // Perform the binary operation based on the operator type (using arith_op.code)
    switch (exp.arith_op.code) {
        case '+':
            code_seq_concat(&seq, code_seq_singleton(code_add(SP, -2, SP, -1)));
            break;
        case '-':
            code_seq_concat(&seq, code_seq_singleton(code_sub(SP, -2, SP, -1)));
            break;
        case '*':
            code_seq_concat(&seq, code_seq_singleton(code_mul(SP, -2)));
            break;
        case '/':
            code_seq_concat(&seq, code_seq_singleton(code_div(SP, -2)));
            break;
        // Add other operators as needed
        default:
            // Handle unknown operator case, maybe return an error or log it
            break;
    }
    // Adjust the stack pointer
    code_seq_concat(&seq, code_seq_singleton(code_ari(SP, 1)));

    return seq;
}
