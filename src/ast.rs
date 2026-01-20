use crate::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub struct Statement;

impl Node for Statement {
    fn token_literal(&self) -> String {
       "".to_string() 
    }
}

impl Statement {
    pub fn statement_node() {}
}

pub struct Expression;

impl Node for Expression {
    fn token_literal(&self) -> String {
       "".to_string() 
    }
}

impl Expression {
    pub fn expression_node() {}
}

struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    fn expression_node() {}
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

struct LexStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl LexStatement {
    fn statement_node() {}
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct Program {
    statements: Vec<Statement>
}

impl Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            let statement = self.statements.get(0).unwrap();
            statement.token_literal()
        } else {
            "".to_string()
        }
    }
}




