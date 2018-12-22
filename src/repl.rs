use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::Eval;
use crate::environment::Environment;
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
        let env = Environment::new();

        loop {
            print!("{} ", PROMPT);
            stdout().flush().unwrap();
            let mut input = String::new();
            match stdin().read_line(&mut input) {
                Ok(_) => {
                    let lexer = Lexer::new(&input);
                    let mut parser = Parser::new(lexer);
                    let program = parser.parse_program();
                    let evaluated = program.eval(env.clone());
                    println!("{}", evaluated);
                }
                Err(error) => println!("Error: {}", error),
            }
        }
    }
}
