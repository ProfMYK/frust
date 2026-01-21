mod lexer;
mod ast;

use lexer::*;
use ast::*;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peak_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser { 
            lexer, 
            cur_token: Token::default(), peak_token: Token::default(),
            errors: Vec::new()
        };
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peak_token.clone();
        self.peak_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.ttype != TokenType::EOF {
            let statement = self.parse_statement();
            if statement.is_some() {
                program.statements.push(statement.unwrap());
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.ttype {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            _ => return None
        }
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt = ReturnStatement::new(self.cur_token.clone());
        self.next_token();

        
        // todo!("Parse the return value expressions!");

        while self.cur_token.ttype != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = LetStatement::new(self.cur_token.clone());
        if !self.expect_peak(TokenType::IDENTIFIER) {
            return None;
        }

        stmt.name = Identifier::new(self.cur_token.clone(), self.cur_token.clone().literal);

        if !self.expect_peak(TokenType::ASSIGN) {
            return None;
        }

        // todo!("Parse the expressions!");

        while self.cur_token.ttype != TokenType::SEMICOLON {
            self.next_token();
        }

        return Some(Box::new(stmt))
    }

    fn expect_peak(&mut self, ttype: TokenType) -> bool {
        if self.peak_token.ttype == ttype {
            self.next_token();
            true
        } else {
            self.peak_error(ttype);
            false
        }
    }

    fn peak_error(&mut self, expected_ttype: TokenType) {
        let msg = format!("Expected next token to be {:?}, but got {:?} instead.", expected_ttype, self.peak_token.ttype);
        self.errors.push(msg.to_string());
    }
}

fn main() {
    let input = "let x = 10;
                 let y = 15;
                 let foobar = 838383;
                 return 5;
                 return add(1, 3);";
    
    let lex = Lexer::new(input.to_string());

    let mut parser = Parser::new(lex);
    
    let program = parser.parse_program();

    println!("{}", program.string());

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
