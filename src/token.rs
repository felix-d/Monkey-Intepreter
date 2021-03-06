#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(String),
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    Minus,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Equal,
    NotEqual,
    Asterik,
    Let,
    Bang,
    Slash,
    Lt,
    Lte,
    Gt,
    Gte,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn literal(&self) -> String {
        let literal = match self {
            Token::Illegal(literal) => literal,
            Token::Ident(literal) => literal,
            Token::Int(literal) => literal,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Comma => ",",
            Token::Minus => "-",
            Token::Semicolon => ";",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Function => "fn",
            Token::Equal => "==",
            Token::NotEqual => "!=",
            Token::Asterik => "*",
            Token::Let => "let",
            Token::Bang => "!",
            Token::Slash => "/",
            Token::Lt => "<",
            Token::Lte => "<=",
            Token::Gt => ">",
            Token::Gte => ">=",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
        };

        literal.to_owned()
    }
}
