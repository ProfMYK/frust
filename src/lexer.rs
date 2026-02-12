use std::fmt::Display;

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Indetifiers + value
    IDENTIFIER,
    INT,

    // Operators
    ASSIGN,    // =
    PLUS,      // +
    MINUS,     // -
    BANG,      // !
    ASTERISK,  // *
    SLASH,     // /

    LT,        // <
    RT,        // >

    EQ,        // ==
    NOTEQ,     // !=

    // DELIMITERS
    COMMA,     // ,
    SEMICOLON, // ;

    LPAREN,    // (
    RPAREN,    // )
    LBRACE,    // {
    RBRACE,    // }
    
    // KEYWORDS
    FUNCTION,  // fun
    LET,       // let
    TRUE,      // true
    FALSE,     // false
    IF,        // if
    ELSE,      // else
    RETURN,    // return
}

#[derive(Clone, Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, ch: String) -> Token {
        Token { ttype: ttype, literal: ch }
    }

    pub fn default() -> Token {
        Token { ttype: TokenType::ILLEGAL, literal: "".to_string() }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type: {:?}, Literal: {}", self.ttype, self.literal)
    }
}

#[derive(Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    pub ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    pub fn _get_tokens(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut token: Token = Token::new(TokenType::ILLEGAL, "".to_string());
        while token.ttype != TokenType::EOF {
            token = self.next_token();
            tokens.push(token.clone());
        }

        token = self.next_token();
        tokens.push(token.clone());

        return tokens
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peak_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token; // = Token::new(TokenType::ILLEGAL, "".to_string());

        self.skip_whitespace();

        match self.ch {
            b'=' => {
                if self.peak_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    token = Token::new(TokenType::EQ, (ch as char).to_string() + &(self.ch as char).to_string());
                } else {
                    token = Token::new(TokenType::ASSIGN, (self.ch as char).to_string());
                }
            },
            b'+' => token = Token::new(TokenType::PLUS, (self.ch as char).to_string()),
            b'-' => token = Token::new(TokenType::MINUS, (self.ch as char).to_string()),
            b'*' => token = Token::new(TokenType::ASTERISK, (self.ch as char).to_string()),
            b'/' => token = Token::new(TokenType::SLASH, (self.ch as char).to_string()),
            b'!' => {
                if self.peak_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    token = Token::new(TokenType::NOTEQ, (ch as char).to_string() + &(self.ch as char).to_string());
                } else {
                    token = Token::new(TokenType::BANG, (self.ch as char).to_string())
                }
            },
            b'<' => token = Token::new(TokenType::LT, (self.ch as char).to_string()),
            b'>' => token = Token::new(TokenType::RT, (self.ch as char).to_string()),
            b';' => token = Token::new(TokenType::SEMICOLON, (self.ch as char).to_string()),
            b',' => token = Token::new(TokenType::COMMA, (self.ch as char).to_string()),
            b'(' => token = Token::new(TokenType::LPAREN, (self.ch as char).to_string()),
            b')' => token = Token::new(TokenType::RPAREN, (self.ch as char).to_string()),
            b'{' => token = Token::new(TokenType::LBRACE, (self.ch as char).to_string()),
            b'}' => token = Token::new(TokenType::RBRACE, (self.ch as char).to_string()),
            0 => token = Token::new(TokenType::EOF, "".to_string()),
            ch if ch.is_ascii_alphabetic() || ch == b'_' => {
                let ident = self.read_identifier();
                token = Token::new(Lexer::lookup_ident(ident.clone()), ident);
                return token;
            },
            ch if ch.is_ascii_digit() => {
                let number = self.read_number();
                token = Token::new(TokenType::INT, number);
            },
            _ => {
                token = Token::new(TokenType::ILLEGAL, (self.ch as char).to_string());
            }
        }

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        self.input.get(position..self.position).expect("Well something went horribly wrong, oops!").to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let literal = self.input.get(position..self.position).expect("Well something went horribly wrong, oops!").to_string();

        self.position -= 1;
        self.read_position -= 1;

        literal
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn lookup_ident(ident: String) -> TokenType {
        match ident {
            s if s == "fun".to_string() => return TokenType::FUNCTION,
            s if s == "let".to_string() => return TokenType::LET,
            s if s == "true".to_string() => return TokenType::TRUE,
            s if s == "false".to_string() => return TokenType::FALSE,
            s if s == "if".to_string() => return TokenType::IF,
            s if s == "else".to_string() => return TokenType::ELSE,
            s if s == "return".to_string() => return TokenType::RETURN,
            _ => return TokenType::IDENTIFIER
        }
    }
}
