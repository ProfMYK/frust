use std::fmt;

use crate::ast::Node;

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum Object{
    Integer(i32),
    Boolean(bool),
    Null,
}

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

fn boolean_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program { statements } => eval_statements(statements),
        Node::ExpressionStatement { expression } => eval(*expression.unwrap()),
        Node::IntegerLiteral { value } => Object::Integer(value),
        Node::BooleanExpression { value } => {
            if value {
                TRUE
            } else {
                FALSE
            }
        },
        Node::PrefixExpression { operator, right } => {
            let right_eval = eval(*right.clone().unwrap());
            eval_prefix_expression(operator, right_eval)
        },
        Node::InfixExpression { left, operator, right } => {
            let right_eval = eval(*right.clone().unwrap());
            let left_eval  = eval(*left.clone().unwrap());
            eval_infix_expression(operator, left_eval, right_eval)
        },
        _ => NULL
    }
}

fn eval_statements(statements: Vec<Node>) -> Object {
    let mut result: Object = NULL;
    for stmt in statements {
        result = eval(stmt);
    }

    result
}

fn eval_infix_expression(op: String, left: Object, right: Object) -> Object {
    if let Object::Integer(left_val) = left && let Object::Integer(right_val) = right {
        return eval_integer_inflix_expression(op, left_val, right_val);
    }
    if let Object::Boolean(left_val) = left && let Object::Boolean(right_val) = right {
        return eval_boolean_inflix_expression(op, left_val, right_val);
    }

    return NULL
}

fn eval_integer_inflix_expression(op: String, left: i32, right: i32) -> Object {
    match op.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => boolean_to_obj(left < right),
        ">" => boolean_to_obj(left > right),
        "==" => boolean_to_obj(left == right),
        "!=" => boolean_to_obj(left != right),
        _ => NULL
    }
}

fn eval_boolean_inflix_expression(op: String, left: bool, right: bool) -> Object {
    match op.as_str() {
        "==" => boolean_to_obj(left == right),
        "!=" => boolean_to_obj(left != right),
        _ => NULL
    }
}

fn eval_prefix_expression(op: String, right: Object) -> Object {
    match op.as_str() {
        "!" => eval_band_operator(right),
        "-" => eval_minus_prefix_operator(right),
        _ => NULL
    }
}

fn eval_minus_prefix_operator(exp: Object) -> Object {
    match exp {
        Object::Integer(val) => {
            Object::Integer(-val)
        },
        _ => NULL
    }
}

fn eval_band_operator(exp: Object) -> Object {
    match exp {
        Object::Boolean(val) => {
            if val {
                FALSE
            } else {
                TRUE
            }
        },
        Object::Null => TRUE,
        _ => FALSE
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "Null")
        }
    }
}
