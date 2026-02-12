mod lexer;
mod ast;
mod parser;
mod repl;
mod evaluator;

use lexer::*;
// use ast::*;
// use parser::*;
use repl::*;

fn main() {
    // let input = "
    //             if (aminake == yarrak) {
    //                 return x / y * (z * c);
    //             let a = 20;
    //             }
    //             let a = 20;
    //             let a = 20;
    //             ";
    //
    // let lex = Lexer::new(input.to_string());
    //
    // let mut parser = Parser::new(lex);
    //
    // let program = parser.parse_program();
    //
    // println!("{}\n", program.string());
    // let mut new_lex = Lexer::new(input.to_string());
    // let tokens = new_lex._get_tokens();
    // for (i, token) in tokens.iter().enumerate() {
    //     println!("{}: {}", i, token.literal);
    // }
    // println!("\nStatements count: {}", program.statements.len());
    //
    // for msg in parser.errors {
    //     println!("ERROR: {}", msg);
    // }
    //
    start();

    // loop {
    //     print!("> ");
    //     io::stdout().flush().unwrap();
    //
    //     let mut input = String::new();
    //     io::stdin().read_line(&mut input).expect("Failed to read line!");
    //     let command = input.trim();
    //
    //     match command {
    //         "exit" | "quit" => break,
    //         "" => continue,
    //         _ => {
    //             let mut lexer = Lexer::new(command.to_string());
    //             let mut token = Token::new(TokenType::ILLEGAL, "".to_string());
    //             while token.ttype != TokenType::EOF {
    //                 token = lexer.next_token();
    //                 println!("{}", token)
    //             }
    //         }
    //     }
    // }
}
