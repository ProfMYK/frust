use std::collections::{HashMap, HashSet};

use crate::ast::{Node};

use crate::{object};
use crate::object::*;

#[macro_export]
macro_rules! check_error {
    ($object:expr) => {
        if matches!($object, Object::Error {..}) {
            return $object;
        }
    };
}

pub fn eval(node: Node, env: EnvRef) -> Object {
    match node {
        Node::Program { statements } => eval_program(statements, env.clone()),
        Node::ExpressionStatement { expression } => eval(*expression.unwrap(), env.clone()),
        Node::IntegerLiteral { value } => Object::Integer(value),
        Node::FloatLiteral { value } => Object::Float(value),
        Node::StringLiteral { value } => Object::String(value),
        Node::BooleanExpression { value } => boolean_to_obj(value),
        Node::PrefixExpression { operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env.clone());
            check_error!(right_eval);

            eval_prefix_expression(operator, right_eval)
        },
        Node::InfixExpression { left, operator, right } => {
            let right_eval = eval(*right.clone().unwrap(), env.clone());
            check_error!(right_eval);

            let left_eval  = eval(*left.clone().unwrap(), env.clone());
            check_error!(right_eval);

            eval_infix_expression(operator, left_eval, right_eval)
        },
        Node::BlockStatement { statements } => eval_block_statements(statements, env.clone()),
        Node::IfExpression { condition, consequence, alternative } => {
            let cond = eval(*condition.clone().unwrap(), env.clone());
            check_error!(cond);

            if is_thruty(cond) {
                return eval(*consequence.clone().unwrap(), env.clone());
            } else if alternative.is_some() {
                return eval(*alternative.clone().unwrap(), env.clone());
            }

            NULL
        },
        Node::ReturnStatement { return_value } => {
            let val = eval(*return_value.clone().unwrap(), env.clone());
            check_error!(val);

            Object::ReturnValue { value: Box::new(val) }
        },
        Node::LetStatement { name, value } => {
            let val = eval(*value.clone().unwrap(), env.clone());
            check_error!(val);

            if let Node::Identifier { value } = *name {
                env.borrow_mut().set(value, val);
            } // The else of this is checked in the parser

            NULL
        },
        Node::Identifier { value } => {
            let val = env.borrow().get(&value);
            
            if let Some(Object::Reference { root, path }) = val {
                return read_nested(root, path, env);
            }

            if let Some(obj) = val {
                return obj;
            }

            if let Some(builtin) = get_builtin(&value) {
                return builtin;
            }

            return Object::Error { message: format!("identifier not found: {}", value) }
        },
        Node::FunctionLiteral { name, parameters, body } => {
            let params = parameters;
            let body = *body.clone().unwrap();
            let function = Object::Function { parameters: params, body, env: env.clone() };

            if let Node::Identifier { value } = *name {
                env.borrow_mut().set(value, function.clone());
            }

            function
        },
        Node::CallExpression { function, arguments } => {
            let func = eval(*function.clone().unwrap(), env.clone());
            check_error!(func);

            let args = eval_expressions(arguments, env.clone());
            if args.len() == 1 {
                check_error!(args[0].clone());
            }

            apply_function(func, args, env)
        },
        Node::ArrayLiteral { elements } => {
            let elems = eval_expressions(elements, env);
            if elems.len() == 1 {
                check_error!(elems[0].clone());
            }
            Object::Array { elements: elems }
        }
        Node::IndexExpression { left, right } => {
            let left_eval = eval(*left.clone().unwrap(), env.clone());
            check_error!(left_eval);

            let right_eval = eval(*right.clone().unwrap(), env);
            check_error!(right_eval);

            eval_index_expression(left_eval, right_eval)
        }
        Node::AssignStatement { left, operator, value } => {
            let right_eval = eval(*value.clone().unwrap(), env.clone());
            check_error!(right_eval);

            match *left.clone() {
                Node::Identifier { value } => {
                    if !env.borrow().contains(&value) {
                        return Object::Error { message: format!("identifier not found: {}", value) }
                    }
                    eval_assign_statement(&value, operator, right_eval, env)
                },
                Node::MemberAccess { .. } | Node::IndexExpression { .. } => {
                    match object::flatten_access_path(*left, env.clone()) {
                        Ok((root_name, path)) => mutate_nested(root_name, path, right_eval, env),
                        Err(err_obj) => err_obj
                    }
                }
                _ => Object::Error { message: format!("assignmnet operator {} not supported for Something I dunno :?", operator) }
            }
        }
        Node::WhileExpession { condition, body } => {
            if let (Some(cond), Some(body_node)) = (condition, body) {
                return while_statement_loop(*cond, *body_node, env);
            } else {
                return Object::Error { 
                    message: format!("Missing condition or body in while loop.") 
                };
            }
        }
        Node::BreakStatement => {
            Object::Error { message: format!("Break Statement not yet implemented sorry :(") }
        }
        Node::StructDefinition { name, fields } => {
            if let Node::Identifier { value } = *name {
                return eval_struct_definition(value, fields, env);
            } // The else of this is checked in the parser

            NULL
        }
        Node::StructLiteral { name, pairs } => {
            if let Node::Identifier { value: name } = *name {
                if let Some(meta) = env.borrow().get(&name) {
                    return eval_struct_literal(meta, name, pairs, env.clone());
                }

                if let Some(meta) = get_builtin(&name) {
                    return eval_struct_literal(meta, name, pairs, env.clone());
                }

                return Object::Error { message: format!("struct {} not found!", name) };
            }
            NULL
        }
        Node::MemberAccess { left, property } => {
            let left_obj = eval(*left.clone(), env.clone());
            check_error!(left_obj);
            match left_obj {
                Object::StructInstance { fields, ..} => {
                    if let Some(val) = fields.get(&property) {
                        return val.clone();
                    } else {
                        return Object::Error { message: format!("property '{}' not found", property) };
                    } 
                },
                Object::Vector2 { x, y } => {
                    if property == "x".to_string() { return Object::Float(x) }
                    if property == "y".to_string() { return Object::Float(y) }
                    return Object::Error { message: format!("property '{}' not found", property) };
                },
                _ => return Object::Error { message: format!("cannot access property on non-struct {}", left_obj) },
            }
        }
        Node::RefStatement { value } => {
            match flatten_access_path(*value, env.clone()) {
                Ok((root, path)) => Object::Reference { root, path },
                Err(err) => err
            }
        }
    }
}

fn while_statement_loop(cond: Node, body: Node, env: EnvRef) -> Object {
    loop {
        let condition_obj = eval(cond.clone(), env.clone());
        check_error!(condition_obj);

        if !is_thruty(condition_obj) {
            break;
        }

        let evaluated = eval(body.clone(), env.clone());
        check_error!(evaluated);
    }

    NULL
}



fn read_nested(root_name: String, path: Vec<AccessStep>, env: EnvRef) -> Object {
    let env_borrow = env.borrow();

    let mut current_obj = match env_borrow.get(&root_name) {
        Some(Object::Reference { root, path }) => read_nested(root, path, env.clone()),
        Some(obj) => obj,
        None => return Object::Error { message: format!("variable {} not found", root_name) },
    };

    if path.len() == 0 { return current_obj; }
    
    for prop in path.iter().take(path.len().saturating_sub(1)) {
        match prop {
            AccessStep::Property(prop) => {
                if let Object::StructInstance { fields, .. } = current_obj {
                    current_obj = match fields.get(prop) {
                        Some(inner) => inner.clone(),
                        None => return Object::Error { message: format!("Property '{}' not found", prop) },
                    };
                } else {
                    return Object::Error { message: format!("Cannot access property on non-struct {}", current_obj.kind()) };
                }
            }
            AccessStep::Index(idx) => {
                if let Object::Array { elements } = current_obj {
                    if *idx >= elements.len() { return Object::Error { message: format!("Index out of bounds") }; }
                    current_obj = elements[*idx].clone();
                } else {
                    return Object::Error { message: format!("cannot index into non-array") };
                }
            }
        }
    }
    
    let final_prop = path.last().unwrap();
    match final_prop {
        AccessStep::Property(prop) => {
            if let Object::StructInstance { fields, .. } = current_obj {
                return match fields.get(prop) {
                    Some(obj) => obj.clone(),
                    None => Object::Error { message: format!("Property '{}' not found", prop) }
                }
            }
        }
        AccessStep::Index(idx) => {
            if let Object::Array { elements } = current_obj {
                if *idx >= elements.len() {
                    return Object::Error { message: format!("Index out of bounds") };
                }
                return elements[*idx].clone();
            }
        }
    }

    Object::Error { message: "Cannot assign to property on non-struct".to_string() }
}

fn mutate_nested(root_name: String, path: Vec<AccessStep>, new_val: Object, env: EnvRef) -> Object {
    let mut env_borrow = env.borrow_mut();

    let mut current_obj = match env_borrow.store.get_mut(&root_name) {
        Some(obj) => obj,
        None => return Object::Error { message: format!("variable {} not found", root_name) },
    };

    for prop in path.iter().take(path.len().saturating_sub(1)) {
        match prop {
            AccessStep::Property(prop) => {
                if let Object::StructInstance { fields, .. } = current_obj {
                    current_obj = match fields.get_mut(prop) {
                        Some(inner) => inner,
                        None => return Object::Error { message: format!("Property '{}' not found", prop) },
                    };
                } else {
                    return Object::Error { message: format!("Cannot access property on non-struct {}", current_obj.kind()) };
                }
            }
            AccessStep::Index(idx) => {
                if let Object::Array { elements } = current_obj {
                    if *idx >= elements.len() { return Object::Error { message: format!("Index out of bounds") }; }
                    current_obj = &mut elements[*idx];
                } else {
                    return Object::Error { message: format!("cannot index into non-array") };
                }
            }
        }
    }
    
    let final_prop = path.last().unwrap();
    match final_prop {
        AccessStep::Property(prop) => {
            if let Object::StructInstance { fields, .. } = current_obj {
                if fields.contains_key(prop) {
                    fields.insert(prop.clone(), new_val);
                    return NULL;
                } else {
                    return Object::Error { message: format!("Property '{}' not found", prop) };
                }
            }
        }
        AccessStep::Index(idx) => {
            if let Object::Array { elements } = current_obj {
                if *idx >= elements.len() {
                    return Object::Error { message: format!("Index out of bounds") };
                }
                elements[*idx] = new_val;
                return NULL;
            }
        }
    }

    Object::Error { message: "Cannot assign to property on non-struct".to_string() }
}

fn eval_struct_literal(meta: Object, name: String, pairs: HashMap<String, Node>, env: EnvRef) -> Object {
    if let Object::StructMeta { name: _name, expected_fields } = meta {
        let expected_set: HashSet<&String> = expected_fields.iter().collect();
        let provided_set: HashSet<&String> = pairs.keys().collect();

        let missing = expected_set.difference(&provided_set).map(|s| (*s).clone()).collect::<Vec<String>>().join(", ");
        let extra = provided_set.difference(&expected_set).map(|s| (*s).clone()).collect::<Vec<String>>().join(", ");

        match (missing.is_empty(), extra.is_empty()) {
            (true, true) => {
                let mut fields = HashMap::new();
                for (key, value) in pairs {
                    let val = eval(value, env.clone());
                    if matches!(val, Object::Error {..}) { return val; }
                    fields.insert(key, val);
                }
                return Object::StructInstance { name, fields }
            },
            (false, true) => return Object::Error { message: format!("missing required fields {} for {}", missing, name) },
            (true, false) => return Object::Error { message: format!("unknown fields {} for {}", extra, name) },
            (false, false) => return Object::Error { message: format!("missing required fields {}, and got unknow fields {}, for {}", missing, extra, name) }
        }
    }

    Object::Error { message: format!("{} is not a struct meta!", name) }
}

fn eval_struct_definition(name: String, fields: Vec<Node>, env: EnvRef) -> Object {
    let mut actual_fields = Vec::new();
    for field in fields.iter() {
        if let Node::Identifier { value: val } = field {
            actual_fields.push(val.to_string());
        } else {
            return Object::Error { message: format!("Expected only identifiers in struct definition {}", name) }
        }
    }

    let meta = Object::StructMeta { name: name.clone(), expected_fields: actual_fields.clone() };
    env.borrow_mut().set(name, meta.clone());
    meta
}

fn eval_assign_statement(name: &str, op: String, val: Object, env: EnvRef) -> Object {
    match op.as_str() {
        "=" => env.borrow_mut().set(name.to_string(), val),
        "+=" => {
            let cur = env.borrow().get(name).unwrap();
            if let Object::Integer(left_val) = val && let Object::Integer(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Integer(left_val + right_val));
            } else if let Object::Float(left_val) = val && let Object::Float(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Float(left_val + right_val));
            } else {
                return Object::Error { message: format!("type mismatch between assignments, expected: {}, got: {}", cur.kind(), val.kind()) }
            }
        },
        "-=" => {
            let cur = env.borrow().get(name).unwrap();
            if let Object::Integer(left_val) = val && let Object::Integer(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Integer(right_val - left_val));
            } else if let Object::Float(left_val) = val && let Object::Float(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Float(right_val - left_val));
            } else {
                return Object::Error { message: format!("type mismatch between assignments, expected: {}, got: {}", cur.kind(), val.kind()) }
            }
        },
        _ => return Object::Error { message: format!("assign operator not found: {}", op) }
    }

    NULL
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

fn apply_function(func: Object, args: Vec<Object>, env: EnvRef) -> Object {
    if let Object::Function { parameters, body, env } = func {
        let extended_env = extend_function_env(args, parameters, env);
        let evaluated = eval(body, extended_env);
        return unwrap_return_value(evaluated);
    }
    if let Object::BuiltinFunction(builtin) = func {
        return builtin(args, env);
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
        check_error!(result);
    }

    result
}

fn eval_infix_expression(op: String, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone()) {
        (Object::Integer(left_val), Object::Integer(right_val)) => eval_integer_infix_expression(op, left_val, right_val),
        (Object::Float(left_val), Object::Float(right_val)) => eval_float_infix_expression(op, left_val, right_val),
        (Object::Integer(left_val), Object::Float(right_val)) => eval_float_infix_expression(op, left_val as f32, right_val),
        (Object::Float(left_val), Object::Integer(right_val)) => eval_float_infix_expression(op, left_val, right_val as f32),
        (Object::Boolean(left_val), Object::Boolean(right_val)) => eval_boolean_infix_expression(op, left_val, right_val),
        (Object::Vector2 { x: left_x, y: left_y }, Object::Vector2 { x: right_x, y: right_y }) => eval_vector2_infix_expression(op, &[left_x, left_y], &[right_x, right_y]),
        (Object::Vector2 { x: left_x, y: left_y }, Object::Float(right_val)) => eval_vector2_infix_expression(op, &[left_x, left_y], &[right_val, right_val]),
        (Object::Vector2 { x: left_x, y: left_y }, Object::Integer(right_val)) => eval_vector2_infix_expression(op, &[left_x, left_y], &[right_val as f32, right_val as f32]),
        (Object::String(ref left_val), Object::String(ref right_val)) => eval_string_infix_expression(op, &left_val, &right_val),
        _ => Object::Error { message: format!("unknown operator {} {} {}", left.kind(), op, right.kind()) },
    }
}

fn eval_vector2_infix_expression(op: String, left: &[f32; 2], right: &[f32; 2]) -> Object {
    match op.as_str() {
        "+" => Object::Vector2{ x: left[0] + right[0], y: left[1] + right[1] },
        "-" => Object::Vector2{ x: left[0] - right[0], y: left[1] - right[1] },
        "*" => Object::Vector2{ x: left[0] * right[0], y: left[1] * right[1] },
        "/" => Object::Vector2{ x: left[0] / right[0], y: left[1] / right[1] },
        "==" => boolean_to_obj(left[0] == right[0] && left[1] == right[1]),
        "!=" => boolean_to_obj(left[0] != right[0] || left[1] != right[1]),
        _ => Object::Error { message: format!("unknown operator Integer {} Integer", op) }
    }
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

fn eval_float_infix_expression(op: String, left: f32, right: f32) -> Object {
    match op.as_str() {
        "+" => Object::Float(left + right),
        "-" => Object::Float(left - right),
        "*" => Object::Float(left * right),
        "/" => Object::Float(left / right),
        "<" => boolean_to_obj(left < right),
        ">" => boolean_to_obj(left > right),
        "==" => boolean_to_obj(left == right),
        "!=" => boolean_to_obj(left != right),
        _ => Object::Error { message: format!("unknown operator Float {} Float", op) }
    }
}

fn eval_boolean_infix_expression(op: String, left: bool, right: bool) -> Object {
    match op.as_str() {
        "==" => boolean_to_obj(left == right),
        "!=" => boolean_to_obj(left != right),
        "&&" => boolean_to_obj(left && right),
        "||" => boolean_to_obj(left || right),
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
        Object::Integer(val) => Object::Integer(-val),
        Object::Float(val) => Object::Float(-val),
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
