use std::collections::HashMap;

use crate::{ast::{BlockStatement, BooleanExpression, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement}, lexer::{Lexer, Token, TokenType}};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(parser: &mut Parser, left: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>>;

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
        TokenType::LPAREN => return Precedence::Call,
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
        p.register_prefix(TokenType::TRUE, Parser::parse_boolean_expression);
        p.register_prefix(TokenType::FALSE, Parser::parse_boolean_expression);
        p.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::IF, Parser::parse_if_expression);
        p.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);

        p.register_infix(TokenType::EQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::NOTEQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::LT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::RT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::PLUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::MINUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression); 
        p.register_infix(TokenType::SLASH, Parser::parse_infix_expression); 
        p.register_infix(TokenType::LPAREN, Parser::parse_call_expression); 

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

        if self.peak_token.ttype == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_expression(&mut self, precedence: i32) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype);
        if prefix == None {
            self.no_prefix_parse_fn_error(self.cur_token.ttype);
            return None;
        }

        let mut left_exp = prefix.unwrap()(self);

        while self.peak_token.ttype != TokenType::SEMICOLON && precedence < self.peak_precedence() {
            let infix = self.infix_parse_fns.get(&self.peak_token.ttype).cloned();
            if infix.is_none() {
                return left_exp;
            }

            self.next_token();

            left_exp = infix.unwrap()(self, left_exp);
        }

        return left_exp;
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = ReturnStatement::new(self.cur_token.clone());
        self.next_token();

        
        stmt.return_value = self.parse_expression(Precedence::Lowest as i32);

        if self.cur_token.ttype == TokenType::SEMICOLON {
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

        self.next_token();

        stmt.value = self.parse_expression(Precedence::Lowest as i32);

        if self.peak_token.ttype == TokenType::SEMICOLON {
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
        let msg = format!("No prefix parse function for {:?} found at {}!", ttype, self.cur_token);
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

    pub fn parse_boolean_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let token = parser.cur_token.clone();
        Some(Box::new(BooleanExpression::new(token.clone(), token.ttype == TokenType::TRUE)))
    }

    pub fn parse_grouped_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        parser.next_token();
        let exp = parser.parse_expression(Precedence::Lowest as i32);
        if !parser.expect_peak(TokenType::RPAREN) {
            return None;
        }

        exp
    }

    pub fn parse_prefix_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let mut exp = PrefixExpression::new(parser.cur_token.clone(), parser.cur_token.clone().literal);
        parser.next_token();
        exp.right = parser.parse_expression(Precedence::Prefix as i32);

        Some(Box::new(exp))
    }

    pub fn parse_if_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let mut exp = IfExpression::new(parser.cur_token.clone());
        if !parser.expect_peak(TokenType::LPAREN) {
            return None;
        }

        parser.next_token();
        exp.condition = parser.parse_expression(Precedence::Lowest as i32);

        if !parser.expect_peak(TokenType::RPAREN) {
            return None;
        }

        if !parser.expect_peak(TokenType::LBRACE) {
            return None;
        }

        exp.consequence = parser.parse_block_statement();

        if parser.peak_token.ttype == TokenType::ELSE {
            parser.next_token();
            if !parser.expect_peak(TokenType::LBRACE) {
                return None;
            }

            exp.alternative = parser.parse_block_statement();
        }

        Some(Box::new(exp))
    }

    pub fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut params = Vec::new();
        if self.peak_token.ttype == TokenType::RPAREN {
            self.next_token();
            return params;
        }

        self.next_token();

        let mut ident = Identifier::new(self.cur_token.clone(), self.cur_token.clone().literal);
        params.push(ident);

        while self.peak_token.ttype == TokenType::COMMA {
            self.next_token();
            self.next_token();
            ident = Identifier::new(self.cur_token.clone(), self.cur_token.clone().literal);
            params.push(ident);
        }

        if !self.expect_peak(TokenType::RPAREN) {
            return Vec::new(); // Return None
        }

        params
    }

    pub fn parse_function_literal(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let mut lit = FunctionLiteral::new(parser.cur_token.clone());
        if !parser.expect_peak(TokenType::LPAREN) {
            return None;
        }
        
        lit.parameters = parser.parse_function_parameters();

        if !parser.expect_peak(TokenType::LBRACE) {
            return None;
        }

        lit.body = parser.parse_block_statement();

        Some(Box::new(lit))
    }

    pub fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block = BlockStatement::new();
        self.next_token();
        
        while self.cur_token.ttype != TokenType::RBRACE && self.cur_token.ttype != TokenType::EOF {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                block.statements.push(stmt.unwrap());
            }
            self.next_token();
            println!("Curr Token: {}", self.cur_token.clone());
        }

        Some(block)
    }

    pub fn parse_infix_expression(parser: &mut Parser, left: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>> {
        let mut exp = InfixExpression::new(parser.cur_token.clone(), parser.cur_token.literal.clone());
        exp.left = left;

        let precedence = parser.cur_precedence();
        parser.next_token();
        exp.right = parser.parse_expression(precedence);

        Some(Box::new(exp))
    }

    pub fn parse_call_arguments(&mut self) -> Vec<Box<dyn Expression>> {
        let mut args = Vec::new();
        if self.peak_token.ttype == TokenType::RPAREN {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest as i32).unwrap());

        while self.peak_token.ttype == TokenType::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest as i32).unwrap());
        }

        if !self.expect_peak(TokenType::RPAREN) {
            return Vec::new();
        }

        args
    }

    pub fn parse_call_expression(parser: &mut Parser, left: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>> {
        let mut exp = CallExpression::new(parser.cur_token.clone());
        exp.arguments = parser.parse_call_arguments();
        exp.function = left;
        Some(Box::new(exp))
    }        
    
}
