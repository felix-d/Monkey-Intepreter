use std::iter::Peekable;
use std::str::CharIndices;

use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    cursor: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.char_indices().peekable();
        let cursor = chars.next();

        Lexer {
            input,
            chars,
            cursor,
        }
    }

    fn read_char(&mut self) {
        self.cursor = self.chars.next();
    }

    fn cursor_position(&self) -> Option<usize> {
        self.cursor.map(|(p, _)| p)
    }

    fn char(&self) -> Option<char> {
        self.cursor.map(|(_, c)| c)
    }

    fn scan<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool,
    {
        let mut any = false;
        while self.chars.peek().map(|(_, c)| f(c)).unwrap_or(false) {
            any = true;
            self.read_char();
        }
        any
    }

    fn scan_once<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool,
    {
        if self.chars.peek().map(|(_, c)| f(c)).unwrap_or(false) {
            self.read_char();
            return true;
        }
        false
    }

    fn get_literal<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(&mut Self) -> (),
    {
        let start_pos = self
            .cursor_position()
            .expect("There must be a character under the cursor.");

        f(self);

        match self.cursor_position() {
            Some(position) => &self.input[start_pos..=position],
            None => &self.input[start_pos..],
        }
        .to_owned()
    }

    fn scan_whitespace(&mut self) {
        self.scan(|c| c.is_whitespace());
    }

    fn scan_alphanumeric(&mut self) {
        self.scan(|c| c.is_alphanumeric());
    }

    fn lex_identifier(&mut self) -> Token {
        let literal: String = self.get_literal(Self::scan_alphanumeric);

        match literal.as_str() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            ident => Token::Ident(ident.to_owned()),
        }
    }

    fn lex_equal(&mut self) -> Token {
        if self.scan_once(|c| *c == '=') {
            Token::Equal
        } else {
            Token::Assign
        }
    }

    fn lex_negation(&mut self) -> Token {
        if self.scan_once(|c| *c == '=') {
            Token::NotEqual
        } else {
            Token::Bang
        }
    }

    fn lex_lt(&mut self) -> Token {
        if self.scan_once(|c| *c == '=') {
            Token::Lte
        } else {
            Token::Lt
        }
    }

    fn lex_gt(&mut self) -> Token {
        if self.scan_once(|c| *c == '=') {
            Token::Gte
        } else {
            Token::Gt
        }
    }

    fn lex_integer(&mut self) -> Token {
        let literal = self.get_literal(|lexer| {
            lexer.scan(|c| c.is_numeric() || *c == '.');
        });

        match literal.parse::<i32>() {
            Ok(_) => Token::Int(literal),
            Err(_) => Token::Illegal(literal),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let token = match self.char()? {
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '-' => Token::Minus,
            '/' => Token::Slash,
            '*' => Token::Asterik,
            '<' => self.lex_lt(),
            '>' => self.lex_gt(),
            '=' => self.lex_equal(),
            '!' => self.lex_negation(),
            'a'...'z' | 'A'...'Z' | '_' => self.lex_identifier(),
            '0'...'9' => self.lex_integer(),
            a if a.is_whitespace() => {
                self.scan_whitespace();
                self.read_char();
                return self.next_token();
            }
            a => Token::Illegal(a.to_string()),
        };
        self.read_char();
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::Token::*;

    #[test]
    fn test_scenario() {
        let input = r##"
          let a = 5;

          let add = fn(x, y) {
            x + y;
          };

          let result = add(five, ten);

          !-/*5;

          5 < 10 > 5;

          if (5 < 10) {
              return true;
          } else {
              return false;
          }

          10 == 10;
          10 != 9;
          10 >= 9;
          10 <= 9;
        "##;

        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Let,
            Ident(String::from("a")),
            Assign,
            Int(String::from("5")),
            Semicolon,
            Let,
            Ident(String::from("add")),
            Assign,
            Function,
            LParen,
            Ident(String::from("x")),
            Comma,
            Ident(String::from("y")),
            RParen,
            LBrace,
            Ident(String::from("x")),
            Plus,
            Ident(String::from("y")),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident(String::from("result")),
            Assign,
            Ident(String::from("add")),
            LParen,
            Ident(String::from("five")),
            Comma,
            Ident(String::from("ten")),
            RParen,
            Semicolon,
            Bang,
            Minus,
            Slash,
            Asterik,
            Int(String::from("5")),
            Semicolon,
            Int(String::from("5")),
            Lt,
            Int(String::from("10")),
            Gt,
            Int(String::from("5")),
            Semicolon,
            If,
            LParen,
            Int(String::from("5")),
            Lt,
            Int(String::from("10")),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            False,
            Semicolon,
            RBrace,
            Int(String::from("10")), Equal,
            Int(String::from("10")),
            Semicolon,
            Int(String::from("10")),
            NotEqual,
            Int(String::from("9")),
            Semicolon,
            Int(String::from("10")),
            Gte,
            Int(String::from("9")),
            Semicolon,
            Int(String::from("10")),
            Lte,
            Int(String::from("9")),
            Semicolon,
        ];

        for expected in expected_tokens.iter() {
            assert_eq!(Some(expected), lexer.next_token().as_ref());
        }
    }
}
