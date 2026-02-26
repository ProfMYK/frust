use std::{cell::RefCell, rc::Rc};
use raylib::prelude::*;

use crate::object::{Object, NULL, EnvRef, boolean_to_obj};

#[derive(Clone)] 
pub struct WindowHandle {
    pub rl: Rc<RefCell<RaylibHandle>>,
    pub thread: Rc<RaylibThread>,
    pub queue: Rc<RefCell<Vec<DrawCommands>>>,
}

impl PartialEq for WindowHandle {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rl, &other.rl)
    }
}

#[derive(Debug)]
pub enum DrawCommands {
    Clear(Color),
    Circle { position: Vector2, radius: f32, color: Color },
    Rectangle { position: Vector2, size: Vector2, color: Color },
    Line { pos1: Vector2, pos2: Vector2, thickness: f32, color: Color },
    Pixel { position: Vector2, color: Color},
}

pub fn builtin_init_window(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\t USAGE: init_window(width, height, title);", args.len()) };
    }

    let width_obj = args[0].clone();
    let height_obj = args[1].clone();
    let title_obj = args[2].clone();
    if let Object::Integer(width) = width_obj && let Object::Integer(height) = height_obj && let Object::String(title) = title_obj.clone() {
        let (mut rl, thread) = raylib::init()
            .size(width, height)
            .title(&title)
            .log_level(TraceLogLevel::LOG_NONE)
            .msaa_4x()
            .build();

        rl.set_target_fps(60);

        let mut env_ref = env.borrow_mut();
        env_ref.window_handle = Some(WindowHandle {
            rl: Rc::new(RefCell::new(rl)),
            thread: Rc::new(thread),
            queue: Rc::new(RefCell::new(Vec::new())),
        });
        drop(env_ref);

        return NULL;
    }

    Object::Error { message: format!("wrong argument type. got width: {}, height: {}, title: {}, expected, width: Integer, height: Integer, title: Integer", 
        width_obj.kind(), height_obj.kind(), title_obj.kind()) }

}

pub fn builtin_should_close(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 0 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 0\n\t USAGE: should_close();", args.len()) };
    }

    if let Some(handle) = &env.borrow().get_window_handle() {
        return boolean_to_obj(handle.rl.borrow().window_should_close());
    }

    Object::Error { message: format!("window not initilized!") }
}

pub fn builtin_circle(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\t USAGE: circle(position, radius, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2]) {
        (Object::Vector2 { x, y }, Object::Float(radius), Object::Color(color)) => {
             if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Circle {
                    position: Vector2::new(*x, *y),
                    radius: *radius, 
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected circle(Vector2, Float, Color), got ({}, {}, {})", 
                         args[0].kind(), args[1].kind(), args[2].kind()) 
        }
    }
}

pub fn builtin_dt(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 0 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 0\n\t USAGE: dt();", args.len()) };
    }
    if let Some(handle) = &env.borrow().get_window_handle() {
        return Object::Float(handle.rl.borrow().get_frame_time())
    }
    return Object::Error { message: format!("window not initilized!") };
}

pub fn builtin_time(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 0 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 0\n\t USAGE: time();", args.len()) };
    }
    if let Some(handle) = &env.borrow().get_window_handle() {
        return Object::Float(handle.rl.borrow().get_time() as f32)
    }
    return Object::Error { message: format!("window not initilized!") };
}

pub fn builtin_line(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 4 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 4\n\t USAGE: line(position1, position2, thickness, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2], &args[3]) {
        (Object::Vector2 { x: pos_x, y: pos_y }, Object::Vector2 { x: pos1_x, y: pos1_y }, Object::Float(thickness), Object::Color(color)) => {
             if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Line {
                    pos1: Vector2::new(*pos_x, *pos_y),
                    pos2: Vector2::new(*pos1_x, *pos1_y),
                    thickness: *thickness, 
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected circle(Vector2, Vector2, Float, Color), got ({}, {}, {}, {})", 
                         args[0].kind(), args[1].kind(), args[2].kind(), args[3].kind()) 
        }
    }
}

pub fn builtin_rectangle(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\t USAGE: rectangle(position, size, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2]) {
        (Object::Vector2 { x: pos_x, y: pos_y }, Object::Vector2 { x: size_x, y: size_y }, Object::Color(color)) => {
             if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Rectangle {
                    position: Vector2::new(*pos_x, *pos_y),
                    size: Vector2::new(*size_x, *size_y),
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected rectangle(Vector2, Vector2, Color), got ({}, {}, {})", 
                         args[0].kind(), args[1].kind(), args[2].kind()) 
        }
    }
}

pub fn builtin_color(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\tUSAGE: color(r, g, b);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2]) {
        (Object::Integer(r), Object::Integer(g), Object::Integer(b)) => Object::Color(Color::new(*r as u8, *g as u8, *b as u8, 255)),
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected color(Integer, Integer, Integer), got ({}, {}, {})", args[0].kind(), args[1].kind(), args[2].kind()) 
        }
    }
}

pub fn builtin_vec2(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 2 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 2\n\tUSAGE: vec2(x, y);", args.len()) };
    }
    
    match (&args[0], &args[1]) {
        (Object::Float(x), Object::Float(y)) => Object::Vector2 { x: *x, y: *y },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected color(Float, Float), got ({}, {})", args[0].kind(), args[1].kind()) 
        }
    }
}

pub fn builtin_dot(args: Vec<Object>, _env: EnvRef) -> Object {
    if args.len() != 2 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 2\n\tUSAGE: dot(vec1, vec2);", args.len()) };
    }
    
    match (&args[0], &args[1]) {
        (Object::Vector2 { x: left_x, y: left_y }, Object::Vector2 { x: right_x, y: right_y }) => Object::Float(left_x * right_x + left_y * right_y),
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected color(Float, Float), got ({}, {})", args[0].kind(), args[1].kind()) 
        }
    }
}

pub fn builtin_pixel(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 2 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 2\n\t USAGE: pixel(position, color);", args.len()) };
    }
    
    match (&args[0], &args[1]) {
        (Object::Vector2{ x, y }, Object::Color(color)) => {
             if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Pixel {
                    position: Vector2::new(*x, *y),
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected pixel(Vector2, Color), got ({}, {})", args[0].kind(), args[1].kind()) 
        }
    }
}

pub fn builtin_clear(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 1\n\t USAGE: clear(color);", args.len()) };
    }
    
    match &args[0] {
        Object::Color(color) => {
            if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Clear(color.clone()));
            }
            NULL
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected clear(Color), got ({})", args[0].kind()) 
        }
    }
}

pub fn builtin_render_frames(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 0 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 0\n\t USAGE: render_frames();", args.len()) };
    }

    let env_borrow = env.borrow();
    if let Some(handle) = &env_borrow.get_window_handle() {
        let mut rl = handle.rl.borrow_mut();
        let mut queue = handle.queue.borrow_mut();
        if queue.is_empty() { return NULL; }
        let mut d = rl.begin_drawing(&handle.thread);

        for cmd in queue.iter() {
            match cmd {
                DrawCommands::Clear(color) => d.clear_background(*color),
                DrawCommands::Circle { position, radius, color } => d.draw_circle_v(position, *radius, color),
                DrawCommands::Rectangle { position, size, color } => d.draw_rectangle_v(position, size, color),
                DrawCommands::Line { pos1, pos2, thickness, color } => d.draw_line_ex(pos1, pos2, *thickness, color),
                DrawCommands::Pixel { position, color } => d.draw_pixel_v(position, color),
            }
        }

        queue.clear();
    }

    NULL
}

