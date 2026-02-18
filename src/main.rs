mod lexer;
mod ast;
mod parser;
mod repl;
mod evaluator;

use std::{env, fs, process::exit};

use repl::*;

use crate::{evaluator::{Environmet, eval}, lexer::Lexer, parser::Parser};
fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    let command = args.get(0);

    match command {
        Some(com) => {
            match com.as_str() {
                "repl" => start(),
                "com" => {
                    let file = args.get(1);
                    match file {
                        Some(file_pos) => {
                            let content = fs::read_to_string(file_pos);
                            if content.is_err() {
                                println!("File openning error: {}", content.err().unwrap());
                            } else {
                                let code = content.unwrap();
                                let env = Environmet::new();
                                let lexer = Lexer::new(code);
                                let mut parser = Parser::new(lexer);

                                let program = parser.parse_program();
                                if parser.errors.len() != 0 {
                                    print_errors(parser.errors);
                                    exit(1);
                                }
                            
                                let evaluated = eval(program, env.clone());
                                if !matches!(evaluated, evaluator::Object::Null) {
                                    println!("{}", evaluated);
                                }
                            }
                        },
                        None => println!("No File Provided: com \"file_location\""),
                    }
                },
                _ => println!("COMMAND NOT FOUND: repl | com"),
            }
        }
        None => println!("NO COMMAND PROVIDED: repl | com"),
    }
}
