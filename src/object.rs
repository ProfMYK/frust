use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use raylib::prelude::*;

use crate::ast::{Node, indent};
use crate::graphics::*;

#[derive(PartialEq, Clone)]
pub enum Object {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    String(String),
    Color(Color),
    Vector2 {x: f32, y: f32},
    Array {elements: Vec<Object>},
    Function {parameters: Vec<Node>, body: Node, env: EnvRef},
    Builtin(BuiltinFunction),
    ReturnValue {value: Box<Object>},
    Error {message: String},
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Float(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::String(value) => write!(f, "{}", value),
            Object::Color(color) => write!(f, "{:?}", color),
            Object::Vector2 { x, y } => write!(f, "{{x: {}, y: {}}}", x, y),
            Object::Array { elements } => {
                let elems = elements.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", ");
                write!(f, "[{}]", elems)
            },
            Object::Builtin(_) => write!(f, "Builtin Function"),
            Object::Null => Ok(()),
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
    pub fn kind(&self) -> &'static str {
        match self {
            Object::Integer(_) => "Integer",
            Object::Float(_) => "Float",
            Object::Boolean(_) => "Boolean",
            Object::String(_) => "String",
            Object::Vector2{..} => "Vector2",
            Object::Color(_) => "Color",
            Object::Array {..} => "Array",
            Object::Builtin(_) => "Builtin Function",
            Object::ReturnValue{..} => "Return Value",
            Object::Null => "Null",
            Object::Error {..} => "Error",
            Object::Function {..} => "Function",
        }
    }
}

type BuiltinFunction = fn(Vec<Object>, env: EnvRef) -> Object;

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

#[derive(PartialEq, Clone)]
pub struct Environmet {
    pub store: HashMap<String, Object>,
    pub outer: Option<EnvRef>,
    pub window_handle: Option<WindowHandle>,
}

pub type EnvRef = Rc<RefCell<Environmet>>;

impl Environmet {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Environmet {
            store: HashMap::new(),
            outer: None,
            window_handle: None,
        }))
    }

    pub fn new_enclosed(outer: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Environmet { 
            store: HashMap::new(), 
            outer: Some(outer),
            window_handle: None,
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(outer_env) => outer_env.borrow().get(name),
                None => None,
            }
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn contains(&self, name: &str) -> bool {
        if self.store.contains_key(name) {
            return true;
        } else {
            match &self.outer {
                Some(outer_env) => outer_env.borrow().contains(name),
                None => false,
            }
        }
    }

    pub fn get_window_handle(&self) -> Option<WindowHandle> {
        match &self.window_handle {
            Some(handle) => Some(handle.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get_window_handle(),
                None => None,
            }
        }
    }
}

pub fn boolean_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

pub fn builtin_len(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 1", args.len()) };
    }

    match &args[0] {
        Object::String(value) => Object::Integer(value.len() as i32),
        Object::Array {elements} => Object::Integer(elements.len() as i32),
        _ => Object::Error { message: format!("argument to `len` not supported: {}", args[0].kind()) }
    }
}

pub fn builtin_pushed(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 2 {
        return Object::Error { message: format!("wrong number of arguments for pushed(Array, Value). got: {}, want: 2", args.len()) };
    }

    match &args[0] {
        Object::Array {elements} => {
            let mut new_elemets = elements.clone();
            new_elemets.push(args[1].clone());
            return Object::Array { elements: new_elemets };
        }
        _ => Object::Error { message: format!("argument to `pushed` not supported: {}", args[0].kind()) }
    }
}

pub fn builtin_sorted(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments for sorted(Array). got: {}, want: 1", args.len()) };
    }

    match &args[0] {
        Object::Array { elements } => {
            let mut new_elemets = elements.clone();
            new_elemets.sort_by(|a, b| {
                match (a, b) {
                    (Object::Integer(x), Object::Integer(y)) => x.cmp(y),
                    (Object::Float(x), Object::Float(y)) => x.total_cmp(y),
                    _ => std::cmp::Ordering::Equal, 
                }
            });
            return Object::Array { elements: new_elemets }
        },
        _ => Object::Error { message: format!("argument to `sorted` not supported: {}", args[0].kind()) }
    }
}

pub fn builtin_print(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() < 1 {
        return Object::Error { message: format!("requires at least 1 argument, got: {}", args.len()) };
    }

    for arg in args {
        print!("{}", arg);
    }

    NULL
}

pub fn builtin_println(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() < 1 {
        return Object::Error { message: format!("requires at least 1 argument, got: {}", args.len()) };
    }

    for arg in args {
        println!("{}", arg);
    }

    NULL
}

pub fn builtin_as_float(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("requires 1 argument, got: {}", args.len()) };
    }

    match &args[0] {
        Object::Integer(val) => Object::Float(*val as f32),
        Object::Float(val) => Object::Float(*val),
        _ => Object::Error { message: format!("requires an Integer, got: {}", args[0].kind()) }
    }
}

pub fn builtin_as_integer(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("requires 1 argument, got: {}", args.len()) };
    }

    match &args[0] {
        Object::Integer(val) => Object::Integer(*val),
        Object::Float(val) => Object::Integer(*val as i32),
        _ => Object::Error { message: format!("requires an Float, got: {}", args[0].kind()) }
    }
}

pub fn builtin_type(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("requires 1 argument, got: {}", args.len()) };
    }

    Object::String(args[0].kind().to_string())
}

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(builtin_len)),
        "sorted" => Some(Object::Builtin(builtin_sorted)),
        "pushed" => Some(Object::Builtin(builtin_pushed)),
        "print" => Some(Object::Builtin(builtin_print)),
        "println" => Some(Object::Builtin(builtin_println)),
        "as_float" => Some(Object::Builtin(builtin_as_float)),
        "as_integer" => Some(Object::Builtin(builtin_as_integer)),
        "type" => Some(Object::Builtin(builtin_type)),
        "init" => Some(Object::Builtin(builtin_init_window)),
        "circle" => Some(Object::Builtin(builtin_circle)),
        "color" => Some(Object::Builtin(builtin_color)),
        "vec2" => Some(Object::Builtin(builtin_vec2)),
        "dot" => Some(Object::Builtin(builtin_dot)),
        "pixel" => Some(Object::Builtin(builtin_pixel)),
        "rectangle" => Some(Object::Builtin(builtin_rectangle)),
        "clear" => Some(Object::Builtin(builtin_clear)),
        "should_close" => Some(Object::Builtin(builtin_should_close)),
        _ => None,
    }
}
