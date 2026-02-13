use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::ast::{Node, indent};


#[derive(PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Function {parameters: Vec<Node>, body: Node, env: EnvRef},
    ReturnValue {value: Box<Object>},
    Error {message: String},
    Null,
}

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;


#[derive(PartialEq, Eq, Clone)]
pub struct Environmet {
    store: HashMap<String, Object>,
    outer: Option<EnvRef>,
}

pub type EnvRef = Rc<RefCell<Environmet>>;

impl Environmet {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Environmet {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Environmet { 
            store: HashMap::new(), 
            outer: Some(outer),
        }))
    }

    fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(outer_env) => outer_env.borrow().get(name),
                None => None,
            }
        }
    }

    fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

}

fn boolean_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

pub fn eval(node: Node, env: EnvRef) -> Object {
    match node {
        Node::Program { statements } => eval_program(statements, env.clone()),
        Node::ExpressionStatement { expression } => eval(*expression.unwrap(), env.clone()),
        Node::IntegerLiteral { value } => Object::Integer(value),
        Node::StringLiteral { value } => Object::String(value),
        Node::BooleanExpression { value } => {
            if value {
                TRUE
            } else {
                FALSE
            }
        },
        Node::PrefixExpression { operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env.clone());
            if matches!(right_eval, Object::Error {..}) {
                return right_eval;
            }
            eval_prefix_expression(operator, right_eval)
        },
        Node::InfixExpression { left, operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env.clone());
            if matches!(right_eval, Object::Error {..}) {
                return right_eval;
            }
            let left_eval  = eval(*left.clone().unwrap(), env.clone());
            if matches!(left_eval, Object::Error {..}) {
                return left_eval;
            }
            eval_infix_expression(operator, left_eval, right_eval)
        },
        Node::BlockStatement { statements } => eval_block_statements(statements, env.clone()),
        Node::IfExpression { condition, consequence, alternative } => {
            let cond = eval(*condition.clone().unwrap(), env.clone());
            if matches!(cond, Object::Error {..}) {
                return cond;
            }
            if is_thruty(cond) {
                return eval(*consequence.clone().unwrap(), env.clone());
            } else if alternative.is_some() {
                return eval(*alternative.clone().unwrap(), env.clone());
            } else {
                return NULL
            }
        },
        Node::ReturnStatement { return_value } => {
            let val = eval(*return_value.clone().unwrap(), env.clone());
            if matches!(val, Object::Error {..}) {
                return val;
            }
            return Object::ReturnValue { value: Box::new(val) }
        },
        Node::LetStatement { name, value } => {
            let val = eval(*value.clone().unwrap(), env.clone());
            if matches!(val, Object::Error {..}) {
                return val;
            }
            if let Node::Identifier { value } = *name {
                env.borrow_mut().set(value, val);
            }
            NULL
        },
        Node::Identifier { value } => {
            let val = env.borrow().get(&value);
            if val.is_none() {
                return Object::Error { message: format!("identifier not found: {}", value) }
            }
            return val.unwrap();
        },
        Node::FunctionLiteral { parameters, body } => {
            let params = parameters;
            let body = *body.clone().unwrap();
            return Object::Function { parameters: params, body, env: env.clone() }
        },
        Node::CallExpression { function, arguments } => {
            let func = eval(*function.clone().unwrap(), env.clone());
            if matches!(func, Object::Error {..}) {
                return func;
            }
            let args = eval_expressions(arguments, env.clone());
            if args.len() == 1 && matches!(args[0], Object::Error {..}) {
                return args[0].clone();
            }

            apply_function(func, args)
        },
        // _ => NULL
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function { parameters, body, env } = func {
        let extended_env = extend_function_env(args, parameters, env);
        let evaluated = eval(body, extended_env);
        return unwrap_return_value(evaluated);
    }
    return Object::Error { message: format!("not a function: {}", func.kind()) }
}

fn extend_function_env(args: Vec<Object>, params: Vec<Node>, env: EnvRef) -> EnvRef {
    let new_env = Environmet::new_enclosed(env.clone());

    for (i, param) in params.iter().enumerate() {
        if let Node::Identifier { value } = param {
            new_env.borrow_mut().set(value.to_string(), args[i].clone());
        }
    }

    new_env
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue { value } = obj {
        return *value;
    }

    obj

}

fn eval_expressions(exps: Vec<Node>, env: EnvRef) -> Vec<Object> {
    let mut result = Vec::new();
    for exp in exps {
        let evaluated = eval(exp, env.clone());
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

fn eval_block_statements(statements: Vec<Node>, env: EnvRef) -> Object {
    let mut result: Object = NULL;
    for stmt in statements {
        result = eval(stmt, env.clone());
        if matches!(result, Object::ReturnValue{..}) || matches!(result, Object::Error {..}) {
            return result;
        }
    }

    result
}

fn eval_program(statements: Vec<Node>, env: EnvRef) -> Object {
    let mut result: Object = NULL;
    for stmt in statements {
        result = eval(stmt, env.clone());
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
            Object::String(value) => write!(f, "{}", value),
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
            Object::String(_) => "String",
            Object::ReturnValue{..} => "Return Value",
            Object::Null => "Null",
            Object::Error {..} => "Error",
            Object::Function {..} => "Function",
        }
    }
}
