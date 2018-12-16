use crate::lexer::Lexer;
use std::io::{stdin, stdout, Write};

const PROMPT: &str = "Monkey>";

pub struct Repl {}

impl Repl {
    pub fn run() {
        Self::welcome();
        Self::evalloop();
    }

    fn welcome() {
        println!("== The Monkey programming language 0.1 ==\n");
    }

    fn evalloop() {
        loop {
            print!("{} ", PROMPT);
            stdout().flush().unwrap();
            let mut input = String::new();
            match stdin().read_line(&mut input) {
                Ok(_) => {
                    let mut lexer = Lexer::new(&input);
                    while let Some(token) = lexer.next_token() {
                        println!("{:?}", token);
                    }
                }
                Err(error) => println!("Error: {}", error),
            }
        }
    }
}
