use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use raylib::prelude::*;

use crate::ast::{Node, indent};

#[derive(Clone, Debug)] 
pub struct WindowHandle {
    pub rl: Rc<RefCell<RaylibHandle>>,
    pub draw: Rc<RefCell<RaylibDrawHandle>>,
    pub thread: Rc<RaylibThread>,
}

impl PartialEq for WindowHandle {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rl, &other.rl)
    }
}

#[derive(PartialEq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Array {elements: Vec<Object>},
    Window (WindowHandle),
    Function {parameters: Vec<Node>, body: Node, env: EnvRef},
    Builtin(BuiltinFunction),
    ReturnValue {value: Box<Object>},
    Error {message: String},
    Null,
}

type BuiltinFunction = fn(Vec<Object>) -> Object;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;


#[derive(PartialEq, Clone)]
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

    fn contains(&self, name: &str) -> bool {
        if self.store.contains_key(name) {
            return true;
        } else {
            match &self.outer {
                Some(outer_env) => outer_env.borrow().contains(name),
                None => false,
            }
        }
    }

}

fn boolean_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

fn builtin_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 1", args.len()) };
    }

    match &args[0] {
        Object::String(value) => Object::Integer(value.len() as i32),
        Object::Array {elements} => Object::Integer(elements.len() as i32),
        _ => Object::Error { message: format!("argument to `len` not supported: {}", args[0].kind()) }
    }
}


fn builtin_init_window(args: Vec<Object>) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\t USAGE: init_window(width, height, title);", args.len()) };
    }

    let width_obj = args[0].clone();
    let height_obj = args[1].clone();
    let title_obj = args[2].clone();
    if let Object::Integer(width) = width_obj && let Object::Integer(height) = height_obj && let Object::String(title) = title_obj.clone() {
        let (rl, thread) = raylib::init()
            .size(width, height)
            .title(&title)
            .build();

        return Object::Window(WindowHandle {
            rl: Rc::new(RefCell::new(rl)),
            thread: Rc::new(thread),
        })
    }

    Object::Error { message: format!("wrong argument type. got width: {}, height: {}, title: {}, expected, width: Integer, height: Integer, title: Integer", 
        width_obj.kind(), height_obj.kind(), title_obj.kind()) }

}

fn builtin_should_close(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 1\n\t USAGE: should_close(window);", args.len()) };
    }

    if let Object::Window(handle) = args[0].clone() {
        let should_close = handle.rl.borrow().window_should_close();
        return Object::Boolean(should_close);
    }

    Object::Error { message: format!("wrong argument type. got window: {}, expected, window: Window", 
        args[0].kind()) }
}

fn builtin_print(args: Vec<Object>) -> Object {
    if args.len() < 1 {
        return Object::Error { message: format!("requires at least 1 argument, got: {}", args.len()) };
    }

    for arg in args {
        println!("{}", arg);
    }
    NULL
}

fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(builtin_len)),
        "print" => Some(Object::Builtin(builtin_print)),
        "init_window" => Some(Object::Builtin(builtin_init_window)),
        "should_close" => Some(Object::Builtin(builtin_should_close)),
        _ => None,
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
            if let Some(val) = env.borrow().get(&value) {
                return val;
            }

            if let Some(builtin) = get_builtin(&value) {
                return builtin;
            }

            return Object::Error { message: format!("identifier not found: {}", value) }
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
        Node::ArrayLiteral { elements } => {
            let elems = eval_expressions(elements, env);
            if elems.len() == 1 && matches!(elems[0], Object::Error{..}) {
                return elems[0].clone();
            }
            Object::Array { elements: elems }
        }
        Node::IndexExpression { left, right } => {
            let l = eval(*left.clone().unwrap(), env.clone());
            if matches!(l, Object::Error {..}) {
                return l;
            }

            let i = eval(*right.clone().unwrap(), env);
            if matches!(i, Object::Error {..}) {
                return i;
            }

            eval_index_expression(l, i)
        }
        Node::AssignStatement { name, operator, value } => {
            if let Node::Identifier { value: name_value } = *name {
                if !env.borrow().contains(&name_value) {
                    return Object::Error { message: format!("identifier not found: {}", name_value) }
                }

                let val = eval(*value.clone().unwrap(), env.clone());

                return eval_assign_statement(&name_value, operator, val, env.clone());
            }

            NULL
        }
        Node::WhileExpession { condition, body } => {
            if let Some(cond) = condition {
                let mut con = eval(*cond.clone(), env.clone());
                if matches!(con, Object::Error {..}) {
                    return con;
                }
                
                let bod = *body.clone().unwrap();
                while is_thruty(con.clone()) {
                    let evaluated = eval(bod.clone(), env.clone());
                    if matches!(evaluated, Object::Error {..}) {
                        return evaluated;
                    }
                    con = eval(*cond.clone(), env.clone());
                }
                return NULL;

            } else {
                return Object::Error { message: format!("condition not avaiable: {:?}", condition) };
            }
        }
        // _ => NULL
    }
}

fn eval_assign_statement(name: &str, op: String, val: Object, env: EnvRef) -> Object {
    match op.as_str() {
        "=" => {
            env.borrow_mut().set(name.to_string(), val);
            return NULL
        },
        "+=" => {
            let cur = env.borrow().get(name).unwrap();
            if let Object::Integer(left_val) = val && let Object::Integer(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Integer(left_val + right_val));
                return NULL;
            } else {
                return Object::Error { message: format!("type mismatch between assignments, expected: {}, got: {}", cur.kind(), val.kind()) }
            }
        },
        "-=" => {
            let cur = env.borrow().get(name).unwrap();
            if let Object::Integer(left_val) = val && let Object::Integer(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Integer(right_val - left_val));
                return NULL;
            } else {
                return Object::Error { message: format!("type mismatch between assignments, expected: {}, got: {}", cur.kind(), val.kind()) }
            }
        },
        _ => return Object::Error { message: format!("assign operator not found: {}", op) }
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (index, left.clone()) {
        (Object::Integer(i), Object::Array {elements}) => {
            let max = elements.len() - 1;
            if i < 0 || i > max as i32 {
                return Object::Error { message: format!("index out of bounds for: {}, max: {}", i, max)}
            }
            return elements[i as usize].clone();
        },
        _ => Object::Error { message: format!("index operator not supported: {}", left.kind()) }
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function { parameters, body, env } = func {
        let extended_env = extend_function_env(args, parameters, env);
        let evaluated = eval(body, extended_env);
        return unwrap_return_value(evaluated);
    }
    if let Object::Builtin(builtin) = func {
        return builtin(args);
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
        return eval_integer_infix_expression(op, left_val, right_val);
    }
    if let Object::Boolean(left_val) = left && let Object::Boolean(right_val) = right {
        return eval_boolean_infix_expression(op, left_val, right_val);
    }
    if let Object::String(ref left_val) = left && let Object::String(ref right_val) = right {
        return eval_string_infix_expression(op, &left_val, &right_val);
    }

    if left.kind() != right.kind() {
        return Object::Error { message: format!("type mismatch {} {} {}", left.kind(), op, right.kind()) }
    }

    return Object::Error { message: format!("unknown operator {} {} {}", left.kind(), op, right.kind()) }
}
fn eval_string_infix_expression(op: String, left: &str, right: &str) -> Object {
    match op.as_str() {
        "+" => Object::String(left.to_owned() + right),
        _ =>  Object::Error { message: format!("unknown operator: String {} String", op)}
    }
}

fn eval_integer_infix_expression(op: String, left: i32, right: i32) -> Object {
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

fn eval_boolean_infix_expression(op: String, left: bool, right: bool) -> Object {
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
            Object::Array { elements } => {
                let elems = elements.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", ");
                write!(f, "[{}]", elems)
            },
            Object::Window(_) => write!(f, "Window Handle"),
            Object::Builtin(_) => write!(f, "Builtin Function"),
            Object::Null => write!(f, ""),
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
            Object::Array {..} => "Array",
            Object::Window(_) => "Window",
            Object::Builtin(_) => "Builtin Function",
            Object::ReturnValue{..} => "Return Value",
            Object::Null => "Null",
            Object::Error {..} => "Error",
            Object::Function {..} => "Function",
        }
    }
}
