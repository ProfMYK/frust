use std::collections::HashMap;

use crate::{ast::Node, lexer::{Lexer, Token, TokenType}};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Box<Node>>;
type InfixParseFn = fn(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>>;

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
    Assign,
    AndOr,
    Equals,      
    LessGreater, 
    Sum,         
    Product,     
    Prefix,      
    Call,       
    Index,       
    Member,       
}

fn get_precedence(ttype: TokenType) -> Precedence {
    match ttype {
        TokenType::ASSIGN => return Precedence::Assign,
        TokenType::ADDASSIGN => return Precedence::Assign,
        TokenType::SUBASSIGN => return Precedence::Assign,
        TokenType::EQ => return Precedence::Equals,
        TokenType::NOTEQ => return Precedence::Equals,
        TokenType::LT => return Precedence::LessGreater,
        TokenType::RT => return Precedence::LessGreater,
        TokenType::AND => return Precedence::AndOr,
        TokenType::OR => return Precedence::AndOr,
        TokenType::PLUS => return Precedence::Sum,
        TokenType::MINUS => return Precedence::Sum,
        TokenType::ASTERISK => return Precedence::Product,
        TokenType::SLASH => return Precedence::Product,
        TokenType::LPAREN => return Precedence::Call,
        TokenType::LBRACKET => return Precedence::Index,
        TokenType::DOT => return Precedence::Member,
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
        // p.register_prefix(TokenType::STRUCT, Parser::parse_struct_literal);
        p.register_prefix(TokenType::NEW, Parser::parse_new_struct_statement);
        p.register_prefix(TokenType::REF, Parser::parse_ref_statement);
        p.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        p.register_prefix(TokenType::FLOAT, Parser::parse_float_literal);
        p.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::TRUE, Parser::parse_boolean_expression);
        p.register_prefix(TokenType::FALSE, Parser::parse_boolean_expression);
        p.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::IF, Parser::parse_if_expression);
        p.register_prefix(TokenType::WHILE, Parser::parse_while_expression);
        p.register_prefix(TokenType::BREAK, Parser::parse_break_expression);
        p.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);
        p.register_prefix(TokenType::STRING, Parser::parse_string_literal);
        p.register_prefix(TokenType::LBRACKET, Parser::parse_array_literal);

        p.register_infix(TokenType::EQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::NOTEQ, Parser::parse_infix_expression); 
        p.register_infix(TokenType::LT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::RT, Parser::parse_infix_expression); 
        p.register_infix(TokenType::AND, Parser::parse_infix_expression); 
        p.register_infix(TokenType::OR, Parser::parse_infix_expression); 
        p.register_infix(TokenType::PLUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::MINUS, Parser::parse_infix_expression); 
        p.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression); 
        p.register_infix(TokenType::SLASH, Parser::parse_infix_expression); 
        p.register_infix(TokenType::LPAREN, Parser::parse_call_expression); 
        p.register_infix(TokenType::DOT, Parser::parse_member_access_expression); 
        p.register_infix(TokenType::ASSIGN, Parser::parse_assign_statment); 
        p.register_infix(TokenType::ADDASSIGN, Parser::parse_assign_statment); 
        p.register_infix(TokenType::SUBASSIGN, Parser::parse_assign_statment); 
        p.register_infix(TokenType::LBRACKET, Parser::parse_index_expression); 

        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peak_token.clone();
        self.peak_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Node {
        let mut statements = Vec::new();

        while self.cur_token.ttype != TokenType::EOF {
            let statement = self.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            }
            self.next_token();
        }

        Node::Program { statements }
    }

    pub fn parse_statement(&mut self) -> Option<Node> {
        match self.cur_token.ttype {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            TokenType::STRUCT => return self.parse_struct_statement(),
            _ => return self.parse_expression_statement()
        }
    }

    pub fn parse_struct_statement(&mut self) -> Option<Node> {
        self.next_token();

        let name = Node::Identifier { value: self.cur_token.literal.clone() };

        if self.expect_peak(TokenType::LBRACE) {
            let fields = self.parse_expression_list(TokenType::RBRACE);
            
            self.next_token();

            return Some(Node::StructDefinition { name: Box::new(name), fields });
        }

        None
    }


    pub fn parse_expression_statement(&mut self) -> Option<Node> {
        let expression = self.parse_expression(Precedence::Lowest as i32);

        if self.peak_token.ttype == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Node::ExpressionStatement { expression })
    }

    pub fn parse_expression(&mut self, precedence: i32) -> Option<Box<Node>> {
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

    pub fn parse_return_statement(&mut self) -> Option<Node> {
        self.next_token();
        
        let return_value = self.parse_expression(Precedence::Lowest as i32);

        if self.peak_token.ttype == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Node::ReturnStatement { return_value })
    }

    pub fn parse_assign_statment(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>> {
        let op = parser.cur_token.literal.clone();
        parser.next_token();
        let value = parser.parse_expression(Precedence::Lowest as i32);
        Some(Box::new(Node::AssignStatement { left: left.unwrap() , operator: op, value }))
    }

    pub fn parse_let_statement(&mut self) -> Option<Node> {
        if !self.expect_peak(TokenType::IDENTIFIER) {
            return None;
        }

        let name = Node::Identifier { value: self.cur_token.clone().literal };

        if !self.expect_peak(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest as i32);

        if self.peak_token.ttype == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Node::LetStatement { name: Box::new(name) , value })
    }

    pub fn register_prefix(&mut self, ttype: TokenType, fun: PrefixParseFn) {
        self.prefix_parse_fns.insert(ttype, fun);
    }

    pub fn register_infix(&mut self, ttype: TokenType, fun: InfixParseFn) {
        self.infix_parse_fns.insert(ttype, fun);
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

    pub fn parse_identifier(parser: &mut Parser) -> Option<Box<Node>> {
        let token = parser.cur_token.clone();
        Some(Box::new(Node::Identifier { value: token.literal }))
    }

    pub fn parse_ref_statement(parser: &mut Parser) -> Option<Box<Node>> {
        parser.next_token();
        let exp = *parser.parse_expression(Precedence::Lowest as i32).unwrap();

        Some(Box::new(Node::RefStatement { value: Box::new(exp) }))
    }

    pub fn parse_new_struct_statement(parser: &mut Parser) -> Option<Box<Node>> {
        parser.next_token();
        let name = Node::Identifier { value: parser.cur_token.literal.clone() };

        let mut fields: HashMap<String, Node> = HashMap::new();
        if parser.expect_peak(TokenType::LBRACE) {
            parser.next_token();
            let key = parser.cur_token.literal.clone();
            parser.next_token();
            parser.next_token();
            let exp = *parser.parse_expression(Precedence::Lowest as i32).unwrap();
            fields.insert(key, exp);

            while parser.peak_token.ttype == TokenType::COMMA {
                parser.next_token();
                parser.next_token();
                let key = parser.cur_token.literal.clone();
                parser.next_token();
                parser.next_token();
                let exp = *parser.parse_expression(Precedence::Lowest as i32).unwrap();
                fields.insert(key, exp);
            }

            parser.next_token();
        }

        Some(Box::new(Node::StructLiteral { name: Box::new(name), pairs: fields }))
    }

    pub fn parse_integer_literal(parser: &mut Parser) -> Option<Box<Node>> {
        match parser.cur_token.literal.parse::<i32>() {
            Ok(n) => {
                return Some(Box::new(Node::IntegerLiteral { value: n }))
            },
            Err(e) => {
                let msg = format!("Couldn't parse {} as integer, got error {}!", 
                    parser.cur_token.literal, e);
                parser.errors.push(msg);
                return None;
            }
        }
    }

    pub fn parse_float_literal(parser: &mut Parser) -> Option<Box<Node>> {
        match parser.cur_token.literal.parse::<f32>() {
            Ok(n) => {
                return Some(Box::new(Node::FloatLiteral { value: n } ))
            },
            Err(e) => {
                let msg = format!("Couldn't parse {} as float, got error {}!", 
                    parser.cur_token.literal, e);
                parser.errors.push(msg);
                return None;
            }
        }
    }

    pub fn parse_boolean_expression(parser: &mut Parser) -> Option<Box<Node>> {
        let token = parser.cur_token.clone();
        Some(Box::new(Node::BooleanExpression { value: token.ttype == TokenType::TRUE }))
    }

    pub fn parse_break_expression(_parser: &mut Parser) -> Option<Box<Node>> {
        Some(Box::new(Node::BreakStatement))
    }

    pub fn parse_grouped_expression(parser: &mut Parser) -> Option<Box<Node>> {
        parser.next_token();
        let exp = parser.parse_expression(Precedence::Lowest as i32);
        if !parser.expect_peak(TokenType::RPAREN) {
            return None;
        }

        exp
    }

    pub fn parse_prefix_expression(parser: &mut Parser) -> Option<Box<Node>> {
        let token = parser.cur_token.clone();
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix as i32);

        Some(Box::new(Node::PrefixExpression { 
            operator: token.literal, 
            right: right 
        }))
    }

    pub fn parse_block_statement(&mut self) -> Option<Box<Node>> {
        let mut statements = Vec::new();
        self.next_token();
        
        while self.cur_token.ttype != TokenType::RBRACE && 
            self.cur_token.ttype != TokenType::EOF {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                statements.push(stmt.unwrap());
            }
            self.next_token();
        }

        Some(Box::new(Node::BlockStatement { statements }))
    }

    pub fn parse_if_expression(parser: &mut Parser) -> Option<Box<Node>> {
        if !parser.expect_peak(TokenType::LPAREN) {
            return None;
        }

        parser.next_token();
        let condition = parser.parse_expression(Precedence::Lowest as i32);

        if !parser.expect_peak(TokenType::RPAREN) {
            return None;
        }

        if !parser.expect_peak(TokenType::LBRACE) {
            return None;
        }

        let consequence = parser.parse_block_statement();

        let mut alternative = None;
        if parser.peak_token.ttype == TokenType::ELSE {
            parser.next_token();
            if !parser.expect_peak(TokenType::LBRACE) {
                return None;
            }

            alternative = parser.parse_block_statement();
        }

        Some(Box::new(Node::IfExpression { condition, consequence, alternative }))
    }

    pub fn parse_while_expression(parser: &mut Parser) -> Option<Box<Node>> {
        if !parser.expect_peak(TokenType::LPAREN) {
            return None;
        }

        parser.next_token();
        let condition = parser.parse_expression(Precedence::Lowest as i32);

        if !parser.expect_peak(TokenType::RPAREN) {
            return None;
        }

        if !parser.expect_peak(TokenType::LBRACE) {
            return None;
        }

        let body = parser.parse_block_statement();

        Some(Box::new(Node::WhileExpession { condition, body }))
    }

    pub fn parse_function_parameters(&mut self) -> Vec<Node> {
        let mut params = Vec::new();
        if self.peak_token.ttype == TokenType::RPAREN {
            self.next_token();
            return params;
        }

        self.next_token();

        params.push(Node::Identifier { 
            value: self.cur_token.clone().literal 
        });

        while self.peak_token.ttype == TokenType::COMMA {
            self.next_token();
            self.next_token();
            params.push(Node::Identifier { 
                value: self.cur_token.clone().literal 
            });
        }

        if !self.expect_peak(TokenType::RPAREN) {
            return Vec::new(); // Return None
        }

        params
    }

    pub fn parse_function_literal(parser: &mut Parser) -> Option<Box<Node>> {
        parser.next_token();
        let name = Node::Identifier { value: parser.cur_token.literal.clone() };

        if !parser.expect_peak(TokenType::LPAREN) {
            return None;
        }
        
        let parameters = parser.parse_function_parameters();

        if !parser.expect_peak(TokenType::LBRACE) {
            return None;
        }

        let body = parser.parse_block_statement();

        Some(Box::new(Node::FunctionLiteral { name: Box::new(name), parameters, body }))
    }

    pub fn parse_string_literal(parser: &mut Parser) -> Option<Box<Node>> {
        let string = parser.cur_token.literal.clone();
        Some(Box::new(Node::StringLiteral { value: string }))
    }

    pub fn parse_expression_list(&mut self, end_token: TokenType) -> Vec<Node> {
        let mut list = Vec::new();
        if self.peak_token.ttype == end_token {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(*self.parse_expression(Precedence::Lowest as i32).unwrap());

        while self.peak_token.ttype == TokenType::COMMA {
            self.next_token();
            self.next_token();
            list.push(*self.parse_expression(Precedence::Lowest as i32).unwrap());
        }

        if !self.expect_peak(end_token) {
            return Vec::new();
        }

        list
    }

    pub fn parse_array_literal(parser: &mut Parser) -> Option<Box<Node>> {
        let elements = parser.parse_expression_list(TokenType::RBRACKET);
        Some(Box::new(Node::ArrayLiteral { elements }))
    }

    pub fn parse_infix_expression(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>> {
        let token = parser.cur_token.clone();

        let precedence = parser.cur_precedence();
        parser.next_token();
        let right = parser.parse_expression(precedence);

        Some(Box::new(Node::InfixExpression { 
            left, operator: token.literal, right 
        }))
    }

    pub fn parse_call_expression(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>> {
        let arguments = parser.parse_expression_list(TokenType::RPAREN);
        Some(Box::new(Node::CallExpression { function: left, arguments }))
    }        

    pub fn parse_member_access_expression(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>> {
        parser.next_token();
        Some(Box::new(Node::MemberAccess { left: left.unwrap(), property: parser.cur_token.literal.clone() }))
    }

    pub fn parse_index_expression(parser: &mut Parser, left: Option<Box<Node>>) -> Option<Box<Node>> {
        parser.next_token();
        let right = parser.parse_expression(Precedence::Lowest as i32);

        if !parser.expect_peak(TokenType::RBRACKET) {
            return None
        }

        Some(Box::new(Node::IndexExpression { left, right }))
    }        
}
