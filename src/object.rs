use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use raylib::prelude::*;

use crate::ast::{Node, indent};
use crate::{evaluator, graphics::*};

#[derive(Clone)]
pub enum Object {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    String(String),
    Color(Color),
    Vector2 {x: f32, y: f32},
    Array {elements: Vec<Object>},
    StructMeta { name: String, expected_fields: Vec<String> },
    StructInstance { name: String, fields: HashMap<String, Object> },
    Function {parameters: Vec<Node>, body: Node, env: EnvRef},
    BuiltinFunction(BuiltinFunction),
    ReturnValue {value: Box<Object>},
    Reference {root: String, path: Vec<AccessStep>},
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
            Object::StructMeta { name, expected_fields } => {
                let elems = expected_fields.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", ");
                write!(f, "{name} = {{{}}}", elems)
            },
            Object::StructInstance { name, fields } => {
                let elems = fields.iter().map(|(k, v)| format!("{k}: {v}")).collect::<Vec<_>>().join(", ");
                write!(f, "{name} {{{}}}", elems)
            },
            Object::BuiltinFunction(_) => write!(f, "Builtin Function"),
            Object::Null => Ok(()),
            Object::Error { message } => write!(f, "ERROR: {}", message),
            Object::ReturnValue { value } => write!(f, "RETURN: {}", value),
            Object::Reference { root, .. } => write!(f, "Reference: {root}", ),
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
            Object::StructMeta {..} => "Struct Meta",
            Object::StructInstance {..} => "Struct Instance",
            Object::BuiltinFunction(_) => "Builtin Function",
            Object::ReturnValue{..} => "Return Value",
            Object::Reference{..} => "Reference",
            Object::Null => "Null",
            Object::Error {..} => "Error",
            Object::Function {..} => "Function",
        }
    }
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

#[derive(Clone)]
pub enum AccessStep {
    Property(String),
    Index(usize),
}

#[derive(Clone)]
pub struct Environmet {
    pub store: HashMap<String, Object>,
    pub outer: Option<EnvRef>,
    pub window_handle: Option<WindowHandle>,
}

pub type EnvRef = Rc<RefCell<Environmet>>;
type BuiltinFunction = fn(Vec<Object>, env: EnvRef) -> Object;

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

pub fn flatten_access_path(node: Node, env: EnvRef) -> Result<(String, Vec<AccessStep>), Object> {
    match node.clone() {
        Node::Identifier { value } => {
            if let Some(obj) = env.borrow().get(&value) {
                if let Object::Reference { root, path } = obj {
                    return Ok((root, path));
                }
            }

            Ok((value, Vec::new()))
        }
        Node::MemberAccess { left, property } => {
            let (root_path, mut path) = flatten_access_path(*left, env)?;
            path.push(AccessStep::Property(property));
            Ok((root_path, path))
        }
        Node::IndexExpression { left, right } => {
            let (root, mut path) = flatten_access_path(*left.unwrap(), env.clone())?;

            let index_val = evaluator::eval(*right.unwrap(), env);

            if let Object::Integer(idx) = index_val {
                if idx < 0 {
                    return Err(Object::Error { message: "Index cannot be negative".to_string() });
                }
                path.push(AccessStep::Index(idx as usize));
                Ok((root, path))
            } else {
                Err(Object::Error { message: "Array index must be an integer".to_string() })
            }
        }
        _ => Err(Object::Error { message: format!("invalid assign target") })
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

pub fn builtin_sqrt(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("requires 1 argument, got: {}", args.len()) };
    }

    match &args[0] {
        Object::Float(val) => Object::Float(val.sqrt()),
        _ => Object::Error { message: format!("expected Float got {}", args[0].kind()) },
    }
}

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltinFunction(builtin_len)),
        "sorted" => Some(Object::BuiltinFunction(builtin_sorted)),
        "pushed" => Some(Object::BuiltinFunction(builtin_pushed)),
        "print" => Some(Object::BuiltinFunction(builtin_print)),
        "println" => Some(Object::BuiltinFunction(builtin_println)),
        "as_float" => Some(Object::BuiltinFunction(builtin_as_float)),
        "sqrt" => Some(Object::BuiltinFunction(builtin_sqrt)),
        "as_integer" => Some(Object::BuiltinFunction(builtin_as_integer)),
        "type" => Some(Object::BuiltinFunction(builtin_type)),
        "init" => Some(Object::BuiltinFunction(builtin_init_window)),
        "circle" => Some(Object::BuiltinFunction(builtin_circle)),
        "line" => Some(Object::BuiltinFunction(builtin_line)),
        "render" => Some(Object::BuiltinFunction(builtin_render_frames)),
        "dt" => Some(Object::BuiltinFunction(builtin_dt)),
        "time" => Some(Object::BuiltinFunction(builtin_time)),
        "color" => Some(Object::BuiltinFunction(builtin_color)),
        "vec2" => Some(Object::BuiltinFunction(builtin_vec2)),
        "dot" => Some(Object::BuiltinFunction(builtin_dot)),
        "pixel" => Some(Object::BuiltinFunction(builtin_pixel)),
        "rectangle" => Some(Object::BuiltinFunction(builtin_rectangle)),
        "clear" => Some(Object::BuiltinFunction(builtin_clear)),
        "should_close" => Some(Object::BuiltinFunction(builtin_should_close)),
        "Vector2" => Some(Object::StructMeta { 
            name: "Vector2".to_string(), 
            expected_fields: vec!["x".to_string(), "y".to_string()] 
        }),
        _ => None,
    }
}
