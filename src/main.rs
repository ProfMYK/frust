mod lexer;
mod ast;
mod parser;
mod repl;
mod evaluator;

use repl::*;

fn main() {
    // let input = "
    //             5;
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
    //
    start();
}
