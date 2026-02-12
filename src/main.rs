mod lexer;
mod ast;
mod parser;

use lexer::*;
use ast::*;
use parser::*;

fn main() {
    // let input = "let x = 10;
    //              let y = 15;
    //              let foobar = 838383;
    //              return 5;
    //              return add(1, 3);
    //              let myVar = anotherVar;";
    let input = "5 + 5;
                 5 - 5;
                 5 * 5;
                 5 / 5;
                 5 > 5;
                 5 < 5;
                 5 == 5;
                 5 != 5;";
    
    let lex = Lexer::new(input.to_string());

    let mut parser = Parser::new(lex);
    
    let program = parser.parse_program();

    println!("{}", program.string());
    println!("Statements count: {}", program.statements.len());

    for msg in parser.errors {
        println!("ERROR: {}", msg);
    }

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
