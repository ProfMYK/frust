mod lexer;
mod ast;

use lexer::*;
use ast::*;

fn main() {
    let input = "let x = 10;
                 let y = 15;
                 let add = fn(a, b) {
                    return a + b;
                 };";
    
    let mut lex = Lexer::new(input.to_string());
    let tokens = lex.get_tokens();

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
