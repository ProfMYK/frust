use std::{io::{self, Write}};

use crate::{evaluator::{Environmet, eval}, lexer::Lexer, parser::Parser};

enum ReplMode {
    Evaluate,
    Parse,
}

pub fn start() {
    let env = Environmet::new();
    let mut mode = ReplMode::Evaluate;
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line!");
        let command = input.trim();

        match command {
            "exit" | "quit" => break,
            "" => continue,
            "mode" => {
                mode = match mode {
                    ReplMode::Parse => {
                        println!("INFO: Repl mode changed to evaluate!");
                        ReplMode::Evaluate
                    }
                    ReplMode::Evaluate => {
                        println!("INFO: Repl mode changed to parse!");
                        ReplMode::Parse
                    }
                }
            },
            _ => {
                let lexer = Lexer::new(command.to_string());
                let mut parser = Parser::new(lexer);

                let program = parser.parse_program();
                if parser.errors.len() != 0 {
                    print_errors(parser.errors);
                    continue;
                }

                match mode {
                    ReplMode::Evaluate => {
                        let evaluated = eval(program, env.clone());
                        println!("{}", evaluated);
                    } 
                    ReplMode::Parse => {
                        println!("{}", program);
                    }
                }
            }
        }
    }
}

fn print_errors(errors: Vec<String>) {
    for error in errors {
        println!("\t{}", error);
    }
}
