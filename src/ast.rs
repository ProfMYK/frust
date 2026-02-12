use std::{any::Any};

use crate::{Token};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum NodeType {
    BlockStatement,
    Identifier,
    FunctionLiteral,
    CallExpression,
    IntegerLiteral,
    BooleanExpression,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    ExpressionStatement,
    LetStatement,
    ReturnStatement,
    Program,
}

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn ntype(&self) -> NodeType;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

pub struct BlockStatement {
    pub token: Token, // TokenType::LBRACET
    pub statements: Vec<Box<dyn Statement>>
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
    fn string(&self) -> String {
        let mut msg = "".to_string();
        for (i, stmt) in self.statements.iter().map(|s| s.string()).enumerate() {
            if i != self.statements.len() - 1 {
                msg = format!("{msg}\n  {}", stmt);
            } else {
                msg = format!("{msg}{}", stmt);
            }
        }
        msg = format!("{msg}\n");

        msg
    }
    fn ntype(&self) -> NodeType {
        NodeType::BlockStatement
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl BlockStatement {
    pub fn new() -> BlockStatement {
        BlockStatement { token: Token::default(), statements: Vec::new() }
    }
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
    fn ntype(&self) -> NodeType {
        NodeType::Identifier
    }
    fn as_any(&self) -> &dyn Any {
        self
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

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
    fn string(&self) -> String {
        let params: String = self.parameters.iter().map(|p| p.string()).collect::<Vec<_>>().join(", ");
        if let Some(ref body) = self.body {
            return format!("{}({}) {{{}}}", self.token_literal(), params, body.string());
        }

        "".to_string()
    }
    fn ntype(&self) -> NodeType {
        NodeType::FunctionLiteral
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

impl FunctionLiteral {
    pub fn new(token: Token) -> FunctionLiteral {
        FunctionLiteral { token, parameters: Vec::new(), body: None }
    }
}

pub struct CallExpression {
    pub token: Token,
    pub function: Option<Box<dyn Expression>>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
    fn string(&self) -> String {
        let args: String = self.arguments.iter().map(|p| p.string()).collect::<Vec<_>>().join(", ");
        if let Some(ref function) = self.function {
            return format!("{}({})", function.string(), args);
        }

        "".to_string()
    }
    fn ntype(&self) -> NodeType {
        NodeType::CallExpression
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {
    fn expression_node(&self) {}
}

impl CallExpression {
    pub fn new(token: Token) -> CallExpression {
        CallExpression { token, function: None, arguments: Vec::new() }
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i32,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn ntype(&self) -> NodeType {
        NodeType::IntegerLiteral
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i32) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.token.literal.clone()
    }
    fn ntype(&self) -> NodeType {
        NodeType::BooleanExpression
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for BooleanExpression {
    fn expression_node(&self) {}
}

impl BooleanExpression {
    pub fn new(token: Token, value: bool) -> BooleanExpression {
        BooleanExpression { token, value }
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        if let Some(ref expres) = self.right {
            return format!("({}{})", self.operator, expres.string());
        }

        "".to_string()
    }
    fn ntype(&self) -> NodeType {
        NodeType::PrefixExpression
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String) -> PrefixExpression {
        PrefixExpression { token, operator, right: None }
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        if let Some(ref right) = self.right && let Some(ref left) = self.left {
            return format!("({} {} {})", left.string(), self.operator, right.string());
        }

        "".to_string()
    }
    fn ntype(&self) -> NodeType {
        NodeType::InfixExpression
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

impl InfixExpression {
    pub fn new(token: Token, operator: String) -> InfixExpression {
        InfixExpression { token, left:None, operator, right: None }
    }
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Option<Box<dyn Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        if let Some(ref cond) = self.condition && let Some(ref cons) = self.consequence {
            let mut msg = format!("if ({}) {{{}}}", cond.string(), cons.string());
            if let Some(ref alt) = self.alternative {
                msg += &format!(" else {{{}}}", alt.string());
                return msg.to_string();
            }

            return msg;
        }

        "".to_string()
    }
    fn ntype(&self) -> NodeType {
        NodeType::IfExpression
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

impl IfExpression {
    pub fn new(token: Token) -> IfExpression {
        IfExpression { token, condition: None, consequence: None, alternative: None }
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl ExpressionStatement {
    pub fn new(cur_token: Token) -> ExpressionStatement {
        ExpressionStatement { token: cur_token, expression: None }
    }
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
    fn ntype(&self) -> NodeType {
        NodeType::ExpressionStatement
    }
    fn as_any(&self) -> &dyn Any {
        self
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
    fn ntype(&self) -> NodeType {
        NodeType::LetStatement
    }
    fn as_any(&self) -> &dyn Any {
        self
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
    fn ntype(&self) -> NodeType {
        NodeType::ReturnStatement
    }
    fn as_any(&self) -> &dyn Any {
        self
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
    fn ntype(&self) -> NodeType {
        NodeType::Program
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new() }
    }
}
