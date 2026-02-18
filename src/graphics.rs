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
    Circle { x: i32, y: i32, radius: i32, color: Color },
    Rectangle { x: i32, y: i32, width: i32, height: i32, color: Color },
    Pixel { x: i32, y: i32, color: Color},
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
            .build();

        rl.set_trace_log(TraceLogLevel::LOG_NONE);
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

    if let Some(handle) = &env.borrow().window_handle {
        return boolean_to_obj(handle.rl.borrow().window_should_close());
    }

    Object::Error { message: format!("window not initilized!") }
}

pub fn builtin_circle(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 4 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 3\n\t USAGE: circle(x, y, radius, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2], &args[3]) {
        (Object::Integer(x), Object::Integer(y), Object::Integer(radius), Object::Color(color)) => {
             if let Some(handle) = &env.borrow().window_handle {
                handle.queue.borrow_mut().push(DrawCommands::Circle {
                    x: *x, 
                    y: *y, 
                    radius: *radius, 
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected circle(Integer, Integer, Integer, Color), got ({}, {}, {}, {})", args[0].kind(), args[1].kind(), args[2].kind(), args[3].kind()) 
        }
    }
}

pub fn builtin_rectangle(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 5 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 4\n\t USAGE: rectangle(x, y, width, height, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2], &args[3], &args[4]) {
        (Object::Integer(x), Object::Integer(y), Object::Integer(width), Object::Integer(height), Object::Color(color)) => {
             if let Some(handle) = &env.borrow().window_handle {
                handle.queue.borrow_mut().push(DrawCommands::Rectangle {
                    x: *x, 
                    y: *y, 
                    width: *width, 
                    height: *height, 
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected rectangle(Integer, Integer, Integer, Integer, Color), got ({}, {}, {}, {}, {})", 
                         args[0].kind(), args[1].kind(), args[2].kind(), args[3].kind(), args[4].kind()) 
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


pub fn builtin_pixel(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 3 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 2\n\t USAGE: pixel(x, y, color);", args.len()) };
    }
    
    match (&args[0], &args[1], &args[2]) {
        (Object::Integer(x), Object::Integer(y), Object::Color(color)) => {
             if let Some(handle) = &env.borrow().get_window_handle() {
                handle.queue.borrow_mut().push(DrawCommands::Pixel {
                    x: *x, 
                    y: *y, 
                    color: *color,
                });
             }
             return NULL;
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected pixel(Integer, Integer, Color), got ({}, {}, {})", args[0].kind(), args[1].kind(), args[2].kind()) 
        }
    }
}

pub fn builtin_clear(args: Vec<Object>, env: EnvRef) -> Object {
    if args.len() != 1 {
        return Object::Error { message: format!("wrong number of arguments. got: {}, want: 1\n\t USAGE: clear(color);", args.len()) };
    }
    
    match &args[0] {
        Object::Color(color) => {
            if let Some(handle) = &env.borrow().window_handle {
                handle.queue.borrow_mut().push(DrawCommands::Clear(color.clone()));
            }
            NULL
        },
        _ => Object::Error { 
            message: format!("Argument type mismatch. Expected clear(Color), got ({})", args[0].kind()) 
        }
    }
}

pub fn flush_graphics(env: EnvRef) {
    let env_borrow = env.borrow();
    if let Some(handle) = &env_borrow.window_handle {
        let mut rl = handle.rl.borrow_mut();
        let mut queue = handle.queue.borrow_mut();
        if queue.is_empty() { return; }
        let mut d = rl.begin_drawing(&handle.thread);

        for cmd in queue.iter() {
            match cmd {
                DrawCommands::Clear(color) => d.clear_background(*color),
                DrawCommands::Circle { x, y, radius, color } => d.draw_circle(*x, *y, *radius as f32, *color),
                DrawCommands::Rectangle { x, y, width, height, color } => d.draw_rectangle(*x, *y, *width, *height, *color),
                DrawCommands::Pixel { x, y, color } => d.draw_pixel(*x, *y, *color),
            }
        }

        queue.clear();
    }
}

