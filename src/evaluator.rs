use crate::ast::{Node};

use crate::graphics::*;
use crate::object::*;

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
            }

            NULL
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

            apply_function(func, args, env.clone())
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

                return eval_assign_statement(&name_value, operator, val, env);
            } else if let Node::IndexExpression { left: left_node, right: right_node } = *name {
                let namee = *left_node.unwrap().to_owned();

                if let Node::Identifier { value: name_value } = namee {
                    let right = *right_node.unwrap().to_owned();
                    let index = eval(right.clone(), env.clone());
                    let val = eval(*value.clone().unwrap(), env.clone());
                    return eval_index_assign_statment(&name_value, index, operator, val, env);
                }
            }

            NULL
        }
        Node::WhileExpession { condition, body } => {
            if let (Some(cond), Some(body_node)) = (condition, body) {
                loop {
                    let condition_obj = eval(*cond.clone(), env.clone());

                    if let Object::Error { .. } = condition_obj {
                        return condition_obj;
                    }

                    if !is_thruty(condition_obj) {
                        break;
                    }

                    let evaluated = eval(*body_node.clone(), env.clone());

                    flush_graphics(env.clone());

                    if let Object::Error { .. } = evaluated {
                        return evaluated;
                    }
                }

                return NULL;

            } else {
                return Object::Error { 
                    message: format!("Missing condition or body in while loop.") 
                };
            }
        }
        Node::BreakStatement => {
            NULL
        }
        // _ => NULL
    }
}

fn eval_index_assign_statment(name: &str, index: Object, operator: String, value: Object, env: EnvRef) -> Object {
    match operator.as_str() {
        "=" => {
            let mut env_borrow = env.borrow_mut();
            let arr = env_borrow.store.get_mut(name);
            match arr {
                Some(Object::Array { elements }) => {
                    if let Object::Integer(i) = index {
                        let mut new_elements = elements.clone();
                        new_elements[i as usize] = value;
                        env_borrow.set(name.to_string(), Object::Array { elements: new_elements });
                    } else {
                        return Object::Error { message: format!("unsupported index type: {}", index.kind()) }
                    }
                },
                None => return Object::Error { message: format!("array not found: {}", name) },
                _ => return Object::Error { message: format!("array not found: {}", name) }
            }
            return NULL
        },
        _ => return Object::Error { message: format!("assign operator not supported: {}", operator) }
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
            } else if let Object::Float(left_val) = val && let Object::Float(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Float(left_val + right_val));
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
            } else if let Object::Float(left_val) = val && let Object::Float(right_val) = cur.clone() {
                env.borrow_mut().set(name.to_string(), Object::Float(right_val - left_val));
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

fn apply_function(func: Object, args: Vec<Object>, env: EnvRef) -> Object {
    if let Object::Function { parameters, body, env } = func {
        let extended_env = extend_function_env(args, parameters, env);
        let evaluated = eval(body, extended_env);
        return unwrap_return_value(evaluated);
    }
    if let Object::Builtin(builtin) = func {
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
        if matches!(result, Object::Error{..}) {
            return result;
        }
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
        Object::Float(val) => {
            Object::Float(-val)
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
