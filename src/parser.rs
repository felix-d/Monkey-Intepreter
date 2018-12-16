use crate::ast::{BlockStatement, Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

macro_rules! expect_peek {
    ($x:expr, $y:pat) => {
        match $x.peek_token {
            Some($y) => {
                $x.next_token();
            }
            ref tok => panic!("Invalid token: {:?}.", tok),
        }
    };
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

fn precedence(token: &Option<Token>) -> Precedence {
    match token {
        Some(Token::Equal) => Precedence::Equals,
        Some(Token::NotEqual) => Precedence::Equals,
        Some(Token::Lt) => Precedence::LessGreater,
        Some(Token::Gt) => Precedence::LessGreater,
        Some(Token::Plus) => Precedence::Sum,
        Some(Token::Minus) => Precedence::Sum,
        Some(Token::Slash) => Precedence::Product,
        Some(Token::Asterik) => Precedence::Product,
        Some(Token::LParen) => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn peek_precedence(&self) -> Precedence {
        precedence(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        precedence(&self.current_token)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.current_token.is_some() {
            let statement = self.parse_statement();
            program.statements.push(statement);
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        expect_peek!(self, Token::Ident(_));

        let identifier = self.current_token.take().unwrap();

        expect_peek!(self, Token::Assign);

        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Statement::Let {
            name: identifier.literal(),
            value: expression,
        }
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Statement::new_return(expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let mut left_expression = self.parse_left_expression();

        loop {
            match self.peek_token.as_ref() {
                Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Slash)
                | Some(Token::Asterik)
                | Some(Token::Equal)
                | Some(Token::NotEqual)
                | Some(Token::Lt)
                | Some(Token::Gt)
                    if precedence < self.peek_precedence() =>
                {
                    self.next_token();
                    left_expression = self.parse_infix_expression(left_expression);
                },
                Some(Token::LParen) => {
                    self.next_token();
                    left_expression = self.parse_call_expression(left_expression);
                },
                _ => return left_expression,
            }
        }
    }

    fn parse_left_expression(&mut self) -> Expression {
        let token = self.current_token.take().unwrap();
        let literal = token.literal();

        match token {
            Token::Function => self.parse_function_literal(),
            Token::If => self.parse_if_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::Ident(_) => Expression::new_identifier(&literal),
            Token::Int(_) => Expression::new_integer(literal.parse().unwrap()),
            Token::Bang => self.parse_prefix_expression(literal),
            Token::Minus => self.parse_prefix_expression(literal),
            Token::True => Expression::new_boolean(true),
            Token::False => Expression::new_boolean(false),
            _ => panic!(),
        }
    }

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        Expression::new_call_expression(function, self.parse_call_arguments())
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut arguments = vec![];
        if self.peek_token == Some(Token::RParen) {
            self.next_token();
            return arguments;
        }
        self.next_token();
        let argument = self.parse_expression(Precedence::Lowest);
        arguments.push(argument);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();
            let argument = self.parse_expression(Precedence::Lowest);
            arguments.push(argument);
        }

        expect_peek!(self, Token::RParen);

        arguments
    }

    fn parse_function_literal(&mut self) -> Expression {
        expect_peek!(self, Token::LParen);
        let parameters = self.parse_function_parameters();
        expect_peek!(self, Token::LBrace);
        let body = self.parse_block_statement();
        Expression::new_function_literal(parameters, body)
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut parameters = vec![];

        if self.peek_token == Some(Token::RParen) {
            self.next_token();
            return parameters;
        }

        self.next_token();

        let identifier = self.current_token.as_ref().unwrap().literal();
        parameters.push(identifier);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();
            let identifier = self.current_token.as_ref().unwrap().literal();
            parameters.push(identifier);
        }

        expect_peek!(self, Token::RParen);

        parameters
    }

    fn parse_prefix_expression(&mut self, operator: String) -> Expression {
        self.next_token();

        match self.current_token {
            Some(_) => Expression::new_prefix(&operator, self.parse_expression(Precedence::Prefix)),
            None => panic!(),
        }
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);
        println!("{:?}", expr);

        expect_peek!(self, Token::RParen);

        expr
    }

    fn parse_if_expression(&mut self) -> Expression {
        expect_peek!(self, Token::LParen);

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        expect_peek!(self, Token::RParen);
        expect_peek!(self, Token::LBrace);

        let consequence = self.parse_block_statement();

        let alternative = match self.peek_token {
            Some(Token::Else) => {
                self.next_token();
                expect_peek!(self, Token::LBrace);
                Some(self.parse_block_statement())
            }
            _ => None,
        };

        Expression::new_if(condition, consequence, alternative)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = vec![];
        self.next_token();
        while self.current_token.is_some() && self.current_token != Some(Token::RBrace) {
            let statement = self.parse_statement();
            statements.push(statement);
            self.next_token();
        }

        BlockStatement::new(statements)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let precedence = self.current_precedence();
        let operator = self.current_token.as_ref().unwrap().literal();
        self.next_token();

        let right = self.parse_expression(precedence);

        Expression::new_infix(&operator, left, right)
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = self.parse_expression(Precedence::Lowest);

        if let Some(Token::Semicolon) = self.peek_token {
            self.next_token();
        }

        Statement::new_expression(expression)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{BlockStatement, Expression, Identifier, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn init_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    #[test]
    fn parse_let_statements() {
        let input = r"
          let x = 5;
          let y = 10;
          let foobar = zoo;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_let("x", Expression::new_integer(5)),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_let("y", Expression::new_integer(10)),
            program.statements[1]
        );
        assert_eq!(
            Statement::new_let("foobar", Expression::new_identifier("zoo")),
            program.statements[2]
        );
    }

    #[test]
    fn parse_return_statement() {
        let input = r"
            return 5;
            return foobar;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_return(Expression::new_integer(5)),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_return(Expression::new_identifier("foobar")),
            program.statements[1]
        );
    }

    #[test]
    fn parse_identifier_expression() {
        let input = r"
            foobar;
            zooBar;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_expression(Expression::new_identifier("foobar")),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_identifier("zooBar")),
            program.statements[1]
        );
    }

    #[test]
    fn parse_integer_literal_expression() {
        let input = r"
            3;
            33444;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_expression(Expression::new_integer(3)),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_integer(33444)),
            program.statements[1]
        );
    }

    #[test]
    fn test_prefix_operators() {
        let input = r"
            !5;
            -15;
            !true;
            !false;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_expression(Expression::new_prefix("!", Expression::new_integer(5))),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_prefix("-", Expression::new_integer(15))),
            program.statements[1]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_prefix("!", Expression::new_boolean(true))),
            program.statements[2]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_prefix("!", Expression::new_boolean(false))),
            program.statements[3]
        );
    }

    #[test]
    fn test_infix_operators() {
        let input = r"
          5 + 5;
          5 - 5;
          5 * 5;
          5 / 5;
          5 > 5;
          5 < 5;
          5 == 5;
          5 != 5;
          true == false;
          true != false;
          false == false;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "+",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[0]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "-",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[1]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "*",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[2]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "/",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[3]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                ">",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[4]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "<",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[5]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "==",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[6]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "!=",
                Expression::new_integer(5),
                Expression::new_integer(5)
            )),
            program.statements[7]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "==",
                Expression::new_boolean(true),
                Expression::new_boolean(false)
            )),
            program.statements[8]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "!=",
                Expression::new_boolean(true),
                Expression::new_boolean(false)
            )),
            program.statements[9]
        );
        assert_eq!(
            Statement::new_expression(Expression::new_infix(
                "==",
                Expression::new_boolean(false),
                Expression::new_boolean(false)
            )),
            program.statements[10]
        );
    }

    #[test]
    fn test_boolean_expressions() {
        let input = r"
          true;
          false;
          let a = true;
        ";

        let program = init_program(input);

        assert_eq!(
            Statement::new_expression(Expression::new_boolean(true)),
            program.statements[0]
        );

        assert_eq!(
            Statement::new_expression(Expression::new_boolean(false)),
            program.statements[1]
        );

        assert_eq!(
            Statement::new_let("a", Expression::new_boolean(true)),
            program.statements[2]
        );
    }

    #[test]
    fn test_operator_precendence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("false", "false"),
            ("true", "true"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let program = init_program(input);
            assert_eq!(format!("{:?}", program), expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r"if (x < y) { x }";
        let program = init_program(input);
        assert_eq!(1, program.statements.len());

        assert_eq!(
            Statement::new_expression(Expression::new_if(
                Expression::new_infix(
                    "<",
                    Expression::new_identifier("x"),
                    Expression::new_identifier("y")
                ),
                BlockStatement::new(vec![Statement::new_expression(Expression::new_identifier(
                    "x"
                ))]),
                None,
            )),
            program.statements[0],
        )
    }

    #[test]
    fn test_if_expression_with_alternative() {
        let input = r"if (x < y) { x } else { y }";
        let program = init_program(input);
        assert_eq!(1, program.statements.len());

        assert_eq!(
            Statement::new_expression(Expression::new_if(
                Expression::new_infix(
                    "<",
                    Expression::new_identifier("x"),
                    Expression::new_identifier("y")
                ),
                BlockStatement::new(vec![Statement::new_expression(Expression::new_identifier(
                    "x"
                ))]),
                Some(BlockStatement::new(vec![Statement::new_expression(
                    Expression::new_identifier("y")
                )])),
            )),
            program.statements[0],
        )
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = r"fn(x, y) { x + y };";
        let program = init_program(input);
        assert_eq!(1, program.statements.len());
        assert_eq!(
            Statement::new_expression(Expression::new_function_literal(
                vec!["x".to_owned(), "y".to_owned(),],
                BlockStatement::new(vec![Statement::new_expression(Expression::new_infix(
                    "+",
                    Expression::new_identifier("x"),
                    Expression::new_identifier("y"),
                ))])
            )),
            program.statements[0]
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            (r"fn() {};", vec![]),
            (r"fn(x) {};", vec!["x"]),
            (r"fn(x, y) {};", vec!["x", "y"]),
        ];

        for (input, expected) in tests {
            let program = init_program(input);
            match &program.statements[0] {
                Statement::Expression(Expression::FunctionLiteral { parameters, .. }) => {
                    let expected = expected
                        .iter()
                        .map(|lit| lit.to_string())
                        .collect::<Vec<Identifier>>();
                    assert_eq!(&expected, parameters);
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = r"add(1, 2 * 3, 4 + 5)";
        let program = init_program(input);
        assert_eq!(1, program.statements.len());
        assert_eq!(
            Statement::new_expression(Expression::new_call_expression(
                Expression::new_identifier("add"),
                vec![
                    Expression::new_integer(1),
                    Expression::new_infix(
                        "*",
                        Expression::new_integer(2),
                        Expression::new_integer(3),
                    ),
                    Expression::new_infix(
                        "+",
                        Expression::new_integer(4),
                        Expression::new_integer(5),
                    ),
                ]
            )),
            program.statements[0],
        );
    }
}
