use crate::ast::{IntegerLiteral, Node, NodeType, Program, Statement};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}

pub trait Object {
    fn inspect(&self) -> String;
    fn otype(&self) -> ObjectType;
}

pub struct Integer {
    value: i32,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn otype(&self) -> ObjectType {
        ObjectType::INTEGER
    }
}

pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn otype(&self) -> ObjectType {
        ObjectType::BOOLEAN
    }
}

pub struct Null {}
impl Object for Null {
    fn inspect(&self) -> String {
        format!("null")
    }

    fn otype(&self) -> ObjectType {
        ObjectType::NULL
    }
}

pub fn eval(node: Box<dyn Node>) -> Option<Box<dyn Object>> {
    let any_node = node.as_any();

    if let Some(int) = any_node.downcast_ref::<IntegerLiteral>() {
        return Some(Box::new(Integer { value: int.value }));
    } else if let Some(program) = any_node.downcast_ref::<Program>() {
        return eval_statements(program.statements);
    }

    return None
}

fn eval_statements(statements: Vec<Box<dyn Statement>>) -> Option<Box<dyn Object>> {
    let mut result = None;
    for statement in statements {
        result = eval(statement);
    }
    result
}
