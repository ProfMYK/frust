use std::fmt::{self};

#[derive(PartialEq, Debug, Clone)]
pub enum Node {
    BlockStatement {statements: Vec<Node>},
    Identifier {value: String},
    FunctionLiteral {parameters: Vec<Node>, body: Option<Box<Node>>},
    CallExpression {function: Option<Box<Node>>, arguments: Vec<Node>},
    IntegerLiteral {value: i32},
    FloatLiteral {value: f32},
    StringLiteral {value: String},
    ArrayLiteral {elements: Vec<Node>},
    IndexExpression {left: Option<Box<Node>>, right: Option<Box<Node>>},
    BooleanExpression {value: bool},
    PrefixExpression {operator: String, right: Option<Box<Node>>},
    InfixExpression {left: Option<Box<Node>>, operator: String, right: Option<Box<Node>>},
    IfExpression {condition: Option<Box<Node>>, consequence: Option<Box<Node>>, alternative: Option<Box<Node>>},
    WhileExpession {condition: Option<Box<Node>>, body: Option<Box<Node>>},
    BreakStatement,
    ExpressionStatement {expression: Option<Box<Node>>},
    LetStatement {name: Box<Node>, value: Option<Box<Node>>},
    AssignStatement {name: Box<Node>, operator: String, value: Option<Box<Node>>},
    ReturnStatement {return_value: Option<Box<Node>>},
    Program {statements: Vec<Node>}
}

pub fn indent(s: &str) -> String {
    s.lines()
        .map(|line| format!("  {}", line))
        .collect::<Vec<_>>()
        .join("\n")
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::IntegerLiteral { value } => write!(f, "(Integer: {})", value),
            Node::FloatLiteral { value } => write!(f, "(Float: {})", value),
            Node::BooleanExpression { value } => write!(f, "(Boolean: {})", value),
            Node::Identifier { value } => write!(f, "(Identifier: {})", value),
            Node::BlockStatement { statements } => {
                let mut msg = format!("(Block Statement\n");
                for node in statements.iter().take(statements.len() - 1) {
                    msg += &format!("{}\n", indent(&format!("{}", node)));
                }
                msg += &format!("{}\n", indent(&format!("{}", statements[statements.len() - 1])));

                write!(f, "{msg})\n")
            },
            Node::FunctionLiteral { parameters, body } => {
                // let mut msg = format!("fun(");
                let pars = parameters.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", ");
                let msg = format!("(Function Literal: ({pars})\n");
                let block = indent(&format!("{}", body.clone().unwrap()));
                write!(f, "{msg}{}\n)", block)
            }
            Node::CallExpression { function, arguments } => {
                let mut msg = format!("(Call Expression: {}\n", function.clone().unwrap());
                let args = indent(&arguments.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", "));
                msg = format!("{msg}{args}\n)");

                write!(f, "{msg}")
            },
            Node::PrefixExpression { operator, right } => write!(f, "({operator}{})", right.clone().unwrap()),
            Node::InfixExpression { left, operator, right } => {
                let l = &format!("{}", left.clone().unwrap());
                let r = &format!("{}", right.clone().unwrap());
                write!(f, "(Infix: {} {operator} {})", l, r)
            },
            Node::IfExpression { condition, consequence, alternative } => {
                let con = indent(&format!("{}", consequence.clone().unwrap()));
                if alternative.is_some() {
                    let alt = indent(&format!("{}", alternative.clone().unwrap()));
                    let mut msg = format!("(If: {}\n{}", condition.clone().unwrap(), con);
                    msg += &format!("\n)\n(Else \n{}", alt);
                    return write!(f, "{msg}\n)")
                } else {
                    return write!(f, "if ({}) {{ \n{} }}", condition.clone().unwrap(), con);
                }
            },
            Node::WhileExpession { condition, body } => {
                let bod = indent(&format!("{}", body.clone().unwrap()));
                return write!(f, "while ({}) {{ \n{} }}", condition.clone().unwrap(), bod);
            },
            Node::BreakStatement => write!(f, "break"),
            Node::ExpressionStatement { expression } => { 
                if let Some(ref exp) = expression.clone() {
                    return write!(f, "{}", exp);
                }

                return write!(f, "");
            }
            Node::LetStatement { name, value } => write!(f, "let {name} = {};", value.clone().unwrap()),
            Node::AssignStatement { name, operator, value } => write!(f, "{name} {operator} {};", value.clone().unwrap()),
            Node::ReturnStatement { return_value } => write!(f, "return {};", return_value.clone().unwrap()),
            Node::Program { statements } => {
                let mut msg = format!("(Program\n");
                let stats = indent(&statements.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join("\n"));
                msg = format!("{msg}{stats}");

                write!(f, "{msg}\n)")
            }
            Node::StringLiteral { value } => write!(f, "(String: \"{}\")", value),
            Node::ArrayLiteral { elements } => {
                let mut msg = format!("(Array: ");
                let stats = indent(&elements.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", "));
                msg = format!("{msg}[{stats}]");

                write!(f, "{msg})")
            }
            Node::IndexExpression { left, right } => {
                write!(f, "(Index Expression: {}[{}])", left.clone().unwrap(), right.clone().unwrap())
            }
        }
    }
}
