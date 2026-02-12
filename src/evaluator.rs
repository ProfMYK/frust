use std::{collections::HashMap, fmt};

use crate::ast::{Node, indent};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Function {parameters: Vec<Node>, body: Node, env: Environmet},
    ReturnValue {value: Box<Object>},
    Error {message: String},
    Null,
}

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Environmet {
    store: HashMap<String, Object>,
    outer: Option<Box<&mut Environmet>>,
}

impl Environmet {
    pub fn new() -> Environmet {
        Environmet { store: HashMap::new(), outer: None }
    }

    fn get(&self, name: String) -> Option<Object> {
        let obj = self.store.get(&name).cloned();
        if obj.is_none() && self.outer.is_some() {
            return self.outer.as_ref().unwrap().get(name);
        }
        return obj
    }

    fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn new_enclosed(outer: &mut Environmet) -> Environmet {
        let mut env = Environmet::new();
        env.outer = Some(Box::new(outer));
        env
    }
}

fn boolean_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

pub fn eval(node: Node, env: &mut Environmet) -> Object {
    match node {
        Node::Program { statements } => eval_program(statements, env),
        Node::ExpressionStatement { expression } => eval(*expression.unwrap(), env),
        Node::IntegerLiteral { value } => Object::Integer(value),
        Node::BooleanExpression { value } => {
            if value {
                TRUE
            } else {
                FALSE
            }
        },
        Node::PrefixExpression { operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env);
            if matches!(right_eval, Object::Error {..}) {
                return right_eval;
            }
            eval_prefix_expression(operator, right_eval)
        },
        Node::InfixExpression { left, operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env);
            if matches!(right_eval, Object::Error {..}) {
                return right_eval;
            }
            let left_eval  = eval(*left.clone().unwrap(), env);
            if matches!(left_eval, Object::Error {..}) {
                return left_eval;
            }
            eval_infix_expression(operator, left_eval, right_eval)
        },
        Node::BlockStatement { statements } => eval_block_statements(statements, env),
        Node::IfExpression { condition, consequence, alternative } => {
            let cond = eval(*condition.clone().unwrap(), env);
            if matches!(cond, Object::Error {..}) {
                return cond;
            }
            if is_thruty(cond) {
                return eval(*consequence.clone().unwrap(), env);
            } else if alternative.is_some() {
                return eval(*alternative.clone().unwrap(), env);
            } else {
                return NULL
            }
        },
        Node::ReturnStatement { return_value } => {
            let val = eval(*return_value.clone().unwrap(), env);
            if matches!(val, Object::Error {..}) {
                return val;
            }
            return Object::ReturnValue { value: Box::new(val) }
        },
        Node::LetStatement { name, value } => {
            let val = eval(*value.clone().unwrap(), env);
            if matches!(val, Object::Error {..}) {
                return val;
            }
            if let Node::Identifier { value } = *name {
                env.set(value, val);
            }
            NULL
        },
        Node::Identifier { value } => {
            let val = env.get(value.clone());
            if val.is_none() {
                return Object::Error { message: format!("identifier not found: {}", value) }
            }
            return val.unwrap();
        },
        Node::FunctionLiteral { parameters, body } => {
            let params = parameters;
            let body = *body.clone().unwrap();
            return Object::Function { parameters, body, env }
        },
        Node::CallExpression { function, arguments } => {
            let func = eval(*function.clone().unwrap(), env);
            if matches!(func, Object::Error {..}) {
                return func;
            }
            let mut args = eval_expressions(arguments, env);
            if args.len() == 1 && matches!(args[0], Object::Error {..}) {
                return args[0];
            }

            apply_function(func, args)
        }
        // _ => NULL
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function { parameters: _, body, env } = func {
        let extended_env = extend_function_env(parameters, env);
        let evaluated = eval(body, extended_env);
        return unwrap_return_value(evaluated);
    }
    return Object::Error { message: format!("not a function: {}", func.kind()) }
}

fn extend_function_env(params: Vec<Node>, env: &mut Environmet) -> &mut Environmet {
    let mut env = Environmet::new_enclosed(env);
}

fn eval_expressions(exps: Vec<Node>, env: &mut Environmet) -> Vec<Object> {
    let mut result = Vec::new();
    for exp in exps {
        let evaluated = eval(exp, env);
        if matches!(evaluated, Object::Error{..}) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }

    result
}

fn is_thruty(condition: Object) -> bool {
    match condition {
        Object::Boolean(val) => val,
        Object::Null => false,
        _ => true
    }
}

fn eval_block_statements(statements: Vec<Node>, env: &mut Environmet) -> Object {
    let mut result: Object = NULL;
    for stmt in statements {
        result = eval(stmt, env);
        if matches!(result, Object::ReturnValue{..}) || matches!(result, Object::Error {..}) {
            return result;
        }
    }

    result
}

fn eval_program(statements: Vec<Node>, env: &mut Environmet) -> Object {
    let mut result: Object = NULL;
    for stmt in statements {
        result = eval(stmt, env);
        if let Object::ReturnValue { value } = result {
            return *value;
        }
        if matches!(result, Object::Error{..}) {
            return result;
        }
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

    if left.kind() != right.kind() {
        return Object::Error { message: format!("type mismatch {} {} {}", left.kind(), op, right.kind()) }
    }

    return Object::Error { message: format!("unknown operator {} {} {}", left.kind(), op, right.kind()) }
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
        _ => Object::Error { message: format!("unknown operator Integer {} Integer", op) }
    }
}

fn eval_boolean_inflix_expression(op: String, left: bool, right: bool) -> Object {
    match op.as_str() {
        "==" => boolean_to_obj(left == right),
        "!=" => boolean_to_obj(left != right),
        _ => Object::Error { message: format!("unknown operator: {} {} {}", left, op, right) }
    }
}

fn eval_prefix_expression(op: String, right: Object) -> Object {
    match op.as_str() {
        "!" => eval_band_operator(right),
        "-" => eval_minus_prefix_operator(right),
        _ => Object::Error { message: format!("unknown operator: {}{}", op, right.kind()) }
    }
}

fn eval_minus_prefix_operator(exp: Object) -> Object {
    match exp {
        Object::Integer(val) => {
            Object::Integer(-val)
        },
        _ => Object::Error { message: format!("unknown operator -{}", exp.kind()) }
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
        _ => Object::Error { message: format!("unknown operator !{}", exp.kind()) }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "Null"),
            Object::Error { message } => write!(f, "ERROR: {}", message),
            Object::ReturnValue { value } => write!(f, "RETURN: {}", value),
            Object::Function { parameters, body, env: _ } => {
                let pars = parameters.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", ");
                let msg = format!("(Function Literal: ({pars})\n");
                let block = indent(&format!("{}", body.clone()));
                write!(f, "{msg}{}\n)", block)
            }
        }
    }
}

impl Object {
    fn kind(&self) -> &'static str {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::ReturnValue{..} => "Return Value",
            Object::Null => "Null",
            Object::Error {..} => "Error",
            Object::Function {..} => "Function",
        }
    }
}
