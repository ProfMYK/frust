mod lexer;
mod ast;
mod parser;
mod repl;
mod evaluator;

use repl::*;

fn main() {
    let input = "
                let one = fun() {
                    return 1;
                }
                let a = 50 + 40 * 20 + one();
                let b = 40;
                let max = fun(x, y) {
                    if (x > y) {
                        return x;
                    } else {
                        return y;
                    }
                }

                print(max(a, b));
                ";

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
