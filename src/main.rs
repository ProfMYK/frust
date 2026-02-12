mod lexer;
mod ast;
mod parser;
mod repl;
mod evaluator;

use repl::*;



fn main() {
    // let input = "
    //             let max = fun(x, y) {
    //                 if (x > y) {
    //                     return add(x, y);
    //                 } else {
    //                     return (y + x);
    //                 }
    //             }
    //
    //             print(max(10, 30));
    //
    //             print((30 + 10) / (293 + 123 - 23 / (1259 - 34)));
    //
    //             let foo = max(2349, 38594);
    //             a + b;
    //             ";
    //
    // let lex = Lexer::new(input.to_string());
    //
    // let mut parser = Parser::new(lex);
    //
    // let program = parser.parse_program();
    //
    // println!("{}\n", program);
    // // println!("\nStatements count: {}", program.statements.len());
    //
    // for msg in parser.errors {
    //     println!("ERROR: {}", msg);
    // }

    start();
}
