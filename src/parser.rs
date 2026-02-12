use std::collections::HashMap;

use crate::{ast::{Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement}, lexer::{Lexer, Token, TokenType}};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(parser: &mut Parser, exp: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>>;

pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: Token,
    pub peak_token: Token,
    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

enum Precedence {
    Lowest = 1,  
    Equals,      
    LessGreater, 
    Sum,         
    Product,     
    Prefix,      
    Call,       
}

fn get_precedence(ttype: TokenType) -> Precedence {
    match ttype {
        TokenType::EQ => return Precedence::Equals,
        TokenType::NOTEQ => return Precedence::Equals,
        TokenType::LT => return Precedence::LessGreater,
        TokenType::RT => return Precedence::LessGreater,
        TokenType::PLUS => return Precedence::Sum,
        TokenType::MINUS => return Precedence::Sum,
        TokenType::ASTERISK => return Precedence::Product,
        TokenType::SLASH => return Precedence::Product,
        _ => Precedence::Lowest,
    }
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser { 
            lexer, 
            cur_token: Token::default(), peak_token: Token::default(),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(), infix_parse_fns: HashMap::new()
        };

        p.next_token();
        p.next_token();

        p.register_prefix(TokenType::IDENTIFIER, Parser::parse_identifier);
        p.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        p.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);

        p.register_infix(TokenType::EQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::NOTEQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::LT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::RT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::PLUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::MINUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression); 
        p.register_infix(TokenType::SLASH, Parser::parse_infix_expression); 

        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peak_token.clone();
        self.peak_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
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

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.ttype {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            _ => return self.parse_expression_statement()
        }
    }

    pub fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = ExpressionStatement::new(self.cur_token.clone());
        stmt.expression = self.parse_expression(Precedence::Lowest as i32);

        println!("Current Token = {}", self.cur_token);

        if self.peak_token.ttype == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_expression(&mut self, _precedence: i32) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype);
        if prefix == None {
            self.no_prefix_parse_fn_error(self.cur_token.ttype);
            return None;
        }

        let left_exp = prefix.unwrap()(self);

        return left_exp;
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt = ReturnStatement::new(self.cur_token.clone());
        self.next_token();

        
        // todo!("Parse the return value expressions!");

        while self.cur_token.ttype != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
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

    pub fn expect_peak(&mut self, ttype: TokenType) -> bool {
        if self.peak_token.ttype == ttype {
            self.next_token();
            true
        } else {
            self.peak_error(ttype);
            false
        }
    }

    pub fn peak_error(&mut self, expected_ttype: TokenType) {
        let msg = format!("Expected next token to be {:?}, but got {:?} instead.", expected_ttype, self.peak_token.ttype);
        self.errors.push(msg.to_string());
    }

    pub fn peak_precedence(&self) -> i32 {
        return get_precedence(self.peak_token.ttype) as i32;
    }

    pub fn cur_precedence(&self) -> i32 {
        return get_precedence(self.cur_token.ttype) as i32;
    }

    pub fn no_prefix_parse_fn_error(&mut self, ttype: TokenType) {
        let msg = format!("No prefix parse function for {:?} found!", ttype);
        self.errors.push(msg);
    }

    pub fn register_prefix(&mut self, ttype: TokenType, fun: PrefixParseFn) {
        self.prefix_parse_fns.insert(ttype, fun);
    }

    pub fn register_infix(&mut self, ttype: TokenType, fun: InfixParseFn) {
        self.infix_parse_fns.insert(ttype, fun);
    }

    pub fn parse_identifier(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let token = parser.cur_token.clone();
        Some(Box::new(Identifier::new(token.clone(), token.literal)))
    }

    pub fn parse_integer_literal(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let mut lit = IntegerLiteral::new(parser.cur_token.clone(), 0);

        match parser.cur_token.literal.parse::<i32>() {
            Ok(n) => {
                lit.value = n;
                return Some(Box::new(lit))
            },
            Err(e) => {
                let msg = format!("Couldn't parse {} as integer, got error {}!", parser.cur_token.literal, e);
                parser.errors.push(msg);
                return None;
            }
        }
    }

    pub fn parse_prefix_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let mut exp = PrefixExpression::new(parser.cur_token.clone(), parser.cur_token.clone().literal);
        parser.next_token();
        exp.right = parser.parse_expression(Precedence::Prefix as i32);

        Some(Box::new(exp))
    }

    pub fn parse_infix_expression(parser: &mut Parser, exp: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>> {
        None
    }
    
}
