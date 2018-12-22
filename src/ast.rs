use crate::token::Token;
use std::fmt;

impl Into<InfixOperator> for &Token {
    fn into(self) -> InfixOperator {
        match self {
            Token::Plus => InfixOperator::Add,
            Token::Minus => InfixOperator::Sub,
            Token::Slash => InfixOperator::Div,
            Token::Asterik => InfixOperator::Mult,
            Token::Equal => InfixOperator::Eq,
            Token::NotEqual => InfixOperator::NotEq,
            Token::Gt => InfixOperator::GreaterThan,
            Token::Lt => InfixOperator::LessThan,
            unknown => unreachable!("unknown token: {:?}", unknown),
        }
    }
}

impl Into<PrefixOperator> for &Token {
    fn into(self) -> PrefixOperator {
        match self {
            Token::Minus => PrefixOperator::Neg,
            Token::Bang => PrefixOperator::Not,
            unknown => unreachable!("unknown token: {:?}", unknown),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(crate) enum InfixOperator {
    Add,
    Sub,
    Div,
    Mult,
    LessThan,
    GreaterThan,
    Eq,
    NotEq,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(crate) enum PrefixOperator {
    Not,
    Neg,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOperator::Add => write!(f, "+"),
            InfixOperator::Sub => write!(f, "-"),
            InfixOperator::Div => write!(f, "/"),
            InfixOperator::Mult => write!(f, "*"),
            InfixOperator::LessThan => write!(f, "<"),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::Eq => write!(f, "=="),
            InfixOperator::NotEq => write!(f, "!="),
        }
    }
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOperator::Not => write!(f, "!"),
            PrefixOperator::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Program {
    pub statements: Vec<Statement>,
}

pub(crate) type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement {
    Let { name: Identifier, value: Expression },
    Return(Expression),
    Expression(Expression),
}

impl Statement {
    pub(crate) fn new_let(identifier: &str, value: Expression) -> Self {
        Statement::Let {
            name: String::from(identifier),
            value,
        }
    }

    pub(crate) fn new_return(return_value: Expression) -> Self {
        Statement::Return(return_value)
    }

    pub(crate) fn new_expression(expression: Expression) -> Self {
        Statement::Expression(expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression {
    IntegerLiteral(i64),
    Identifier(Identifier),
    Prefix {
        operator: PrefixOperator,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: InfixOperator,
    },
    Boolean(bool),
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        params: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        args: Vec<Expression>,
    },
}

impl Expression {
    pub(crate) fn new_integer(value: i64) -> Expression {
        Expression::IntegerLiteral(value)
    }

    pub(crate) fn new_identifier(value: &str) -> Self {
        Expression::Identifier(String::from(value))
    }

    pub(crate) fn new_prefix<T>(operator: T, right: Expression) -> Self
    where
        T: Into<PrefixOperator>,
    {
        Expression::Prefix {
            operator: operator.into(),
            right: Box::new(right),
        }
    }

    pub(crate) fn new_infix<T>(operator: T, left: Expression, right: Expression) -> Self
    where
        T: Into<InfixOperator>,
    {
        Expression::Infix {
            left: Box::new(left),
            right: Box::new(right),
            operator: operator.into(),
        }
    }

    pub(crate) fn new_boolean(value: bool) -> Self {
        Expression::Boolean(value)
    }

    pub(crate) fn new_if(
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Expression::IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }

    pub(crate) fn new_function_literal(params: Vec<Identifier>, body: BlockStatement) -> Self {
        Expression::FunctionLiteral { params, body }
    }

    pub(crate) fn new_call_expression(function: Expression, args: Vec<Expression>) -> Self {
        Expression::CallExpression {
            function: Box::new(function),
            args,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BlockStatement(pub Vec<Statement>);

impl BlockStatement {
    pub(crate) fn new(statements: Vec<Statement>) -> Self {
        BlockStatement(statements)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Return(expression) => write!(f, "return {};", expression),
            Statement::Expression(expression) => write!(f, "{}", expression),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::IntegerLiteral(literal) => write!(f, "{}", literal),
            Expression::Identifier(identifier) => write!(f, "{}", identifier),
            Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                right,
                operator,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if ({}) {}", condition, consequence)?;
                match alternative {
                    Some(ref alternative) => write!(f, "else {}", alternative),
                    None => Ok(()),
                }
            }
            Expression::FunctionLiteral { params, body } => {
                let params = params
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<Identifier>>();
                write!(f, "fn ({}) {{{}}}", params.join(", "), body)
            }
            Expression::CallExpression { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<Identifier>>()
                    .join(", ");
                write!(f, "{}({})", function, args)
            }
        }
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statements = self
            .0
            .iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>()
            .join("; ");

        write!(f, "{}", statements)
    }
}
