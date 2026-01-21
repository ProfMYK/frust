use std::any::Any;

use crate::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

pub struct Identifier {
    pub token: Token, // TokenType::IDENT
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier { token, value }
    }
    pub fn default() -> Identifier {
        Identifier { token: Token::default(), value: "".to_string() }
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.clone().literal
    }

    fn string(&self) -> String {
        if let Some(ref expres) = self.expression {
            return expres.string();
        }
        "".to_string()
    }
}

pub struct LetStatement {
    pub token: Token,  // TokenType::LET
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut msg = format!("{} {} = ", self.token_literal(), self.name.string());
        if let Some(ref expr) = self.value {
            msg = format!("{msg}{}", expr.string());
        }
        return msg + ";";
    }
}
impl LetStatement {
    pub fn new(token: Token) -> LetStatement {
        LetStatement { token, name: Identifier::default(), value: None }
    }
}

pub struct ReturnStatement {
    pub token: Token,  // TokenType::RETURN
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut msg = self.token_literal();

        if let Some(ref expr) = self.return_value {
            msg = format!("{msg} {}", expr.string());
        }

        return msg + ";";
    }
}
impl ReturnStatement {
    pub fn new(token: Token) -> ReturnStatement {
        ReturnStatement { token, return_value: None }
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            let statement = self.statements.get(0).unwrap();
            statement.token_literal()
        } else {
            "".to_string()
        }
    }

    fn string(&self) -> String {
        let mut msg = "".to_string();
        for stmt in self.statements.iter() {
            msg = format!("{msg}\n{}", stmt.string());
        }

        msg
    }
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new() }
    }
}
