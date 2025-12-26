use std::{
    fmt::{Display, Write},
    hash::{DefaultHasher, Hash, Hasher},
    io::{self, BufRead, Write as IOWrite},
    str::Chars,
};

// Lexer ------------------

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Hash)]
pub enum Token {
    Lambda,
    Dot,
    LParen,
    RParen,
    Equal, // =

    Ident(String),

    Invalid(char),
    EOL,
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    cur: Option<char>,
    pub pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input: input.chars(),
            cur: None,
            pos: 0,
        };
        lexer.advance();
        lexer
    }

    fn advance(&mut self) {
        self.cur = self.input.next();
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.cur {
            if c.is_whitespace() || c == '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.cur {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    fn next(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.cur {
            Some(c) => match c {
                '\\' | 'λ' => {
                    self.advance();
                    Token::Lambda
                }
                '(' => {
                    self.advance();
                    Token::LParen
                }
                ')' => {
                    self.advance();
                    Token::RParen
                }
                '.' => {
                    self.advance();
                    Token::Dot
                }
                '=' => {
                    self.advance();
                    Token::Equal
                }
                c if c.is_alphanumeric() => Token::Ident(self.read_ident()),
                c => {
                    self.advance();
                    Token::Invalid(c)
                }
            },
            None => Token::EOL,
        };

        token
    }
}

// -----------------------

// Parser ----------------

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let cur = lexer.next();
        let next_token = lexer.next();

        Self {
            lexer,
            cur,
            next_token,
        }
    }

    fn next(&mut self) {
        self.cur = std::mem::replace(&mut self.next_token, self.lexer.next());
        // same as:
        // self.cur = self.next.clone();
        // self.next = self.lexer.next_token();
    }

    fn expect(&mut self, expected: Token) -> Result<Token, String> {
        if self.cur == expected {
            self.next();
            Ok(expected)
        } else {
            Err(format!("Expected: {:?}, got: {:?}", expected, self.cur))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        let ident = if let Token::Ident(name) = self.cur.clone() {
            self.next();
            name
        } else {
            return Err(format!(
                "Error: Unexpected identifier while parsing a function: '{:?}'.",
                self.cur
            ));
        };

        Ok(ident)
    }

    fn parse_fun(&mut self) -> Result<Expr, String> {
        self.expect(Token::Lambda)?;
        let arg = self.expect_ident()?;
        self.expect(Token::Dot)?;

        let body = self.parse_expr()?;

        Ok(Expr::fun(arg, body))
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.cur.clone() {
            Token::LParen => {
                // skipping '('
                self.next();

                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Lambda => self.parse_fun(),
            Token::Ident(name) => {
                self.next();
                Ok(Expr::var(name))
            }
            _ => Err(format!(
                "Error: Unexpected token '{:?}'. Expected a primary expression instead.",
                self.cur
            )),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        while self.cur != Token::EOL && self.cur != Token::RParen {
            let rhs = self.parse_primary()?;
            expr = Expr::app(expr, rhs);
        }

        Ok(expr)
    }

    fn parse_binding(&mut self) -> Result<Binding, String> {
        let name = self.expect_ident()?;
        self.expect(Token::Equal)?;
        let body = self.parse_expr()?;

        Ok(Binding::new(name, body))
    }
}

// -----------------------

// Evaluation ------------

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var { name: String, hash: u64 },
    Fun { arg: String, body: Box<Expr> },
    App { lhs: Box<Expr>, rhs: Box<Expr> },
}

impl Expr {
    fn var(name: impl Into<String> + Clone + Hash) -> Self {
        let mut s = DefaultHasher::new();
        name.clone().hash(&mut s);
        Self::Var {
            name: name.into(),
            hash: s.finish(),
        }
    }

    fn fun(arg: impl Into<String>, body: Expr) -> Self {
        Self::Fun {
            arg: arg.into(),
            body: Box::new(body),
        }
    }

    fn app(lhs: Expr, rhs: Expr) -> Self {
        Self::App {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn eval_one(&self, bindigs: &[Binding]) -> Self {
        match self.clone() {
            Expr::Var { name, .. } => {
                if let Some(binding) = bindigs.iter().find(|&x| x.name == name) {
                    binding.body.clone()
                } else {
                    self.clone()
                }
            }
            Expr::Fun { arg, body } => {
                let evaluated_body = body.eval_one(bindigs);

                if evaluated_body != *body {
                    Expr::fun(arg, evaluated_body)
                } else {
                    self.clone()
                }
            }
            Expr::App { lhs, rhs } => {
                if let Expr::Fun { .. } = *lhs {
                    return reduce(&lhs, &rhs);
                }

                let evaluated_lhs = lhs.eval_one(bindigs);
                if *lhs != evaluated_lhs {
                    return Expr::app(evaluated_lhs, *rhs.clone());
                }

                let evaluated_rhs = rhs.eval_one(bindigs);
                if *rhs != evaluated_rhs {
                    return Expr::app(evaluated_lhs, evaluated_rhs);
                }

                self.clone()
            }
        }
    }

    fn eval(&self, bindings: &[Binding]) -> Expr {
        let mut current = self.clone();
        loop {
            let next = current.eval_one(&bindings);
            if next == current {
                return current;
            }
            current = next;
        }
    }

    fn display_ast(&self) -> String {
        fn dump_ast(expr: &Expr, indent: &[bool]) -> String {
            let prefix: String = indent
                .iter()
                .enumerate()
                .map(|(i, has_sibling)| {
                    if i + 1 == indent.len() {
                        "+--"
                    } else {
                        if *has_sibling { "|  " } else { "   " }
                    }
                })
                .collect();

            match expr {
                Expr::Var { name, .. } => format!("{}[VAR] {}\n", prefix, name),
                Expr::App { lhs, rhs } => {
                    let mut result = format!("{}[APP]\n", prefix);
                    result += &dump_ast(lhs, &[indent, &[true]].concat());
                    result += &dump_ast(rhs, &[indent, &[false]].concat());
                    result
                }
                Expr::Fun { arg, body } => {
                    let mut result = format!("{}[FUN] \\{}\n", prefix, arg);
                    result += &dump_ast(body, &[indent, &[false]].concat());
                    result
                }
            }
        }

        dump_ast(self, &[])
    }
}

fn replace(param: &String, body: &Expr, val: &Expr) -> Expr {
    match body {
        Expr::Var { name, .. } => {
            if name == param {
                val.clone()
            } else {
                body.clone()
            }
        }
        Expr::Fun { arg, body } => {
            if arg == param {
                Expr::fun(arg, *body.clone())
            } else {
                Expr::fun(arg, replace(param, body, val))
            }
        }
        Expr::App { lhs, rhs } => Expr::app(replace(param, &lhs, val), replace(param, &rhs, val)),
    }
}

fn reduce(fun: &Expr, val: &Expr) -> Expr {
    if let Expr::Fun { arg, body } = fun {
        replace(arg, body, val)
    } else {
        unreachable!("Expr kind")
    }
}

// -----------------------

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var { name, .. } => f.write_str(name),
            Expr::Fun { arg, body } => {
                f.write_char('λ')?;
                f.write_str(arg)?;
                f.write_char('.')?;
                body.fmt(f)
            }
            Expr::App { lhs, rhs } => {
                f.write_char('(')?;
                lhs.fmt(f)?;
                f.write_char(' ')?;
                rhs.fmt(f)?;
                f.write_char(')')
            }
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.name, self.body))
    }
}

impl State {
    fn new() -> Self {
        Self { bindings: vec![] }
    }

    fn push_binding(&mut self, binding: Binding) {
        self.bindings.push(binding);
    }

    fn display_bindigs(&self) -> String {
        self.bindings
            .iter()
            .map(|binding| format!("-> {}", binding))
            .collect()
    }
}

pub struct Binding {
    pub name: String,
    pub body: Expr,
    pub hash: u64,
}

impl Binding {
    fn new(name: String, body: Expr) -> Self {
        let mut s = DefaultHasher::new();

        name.hash(&mut s);

        Self {
            name,
            body,
            hash: s.finish(),
        }
    }
}

struct State {
    bindings: Vec<Binding>,
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut state = State::new();

    loop {
        print!("λ> ");

        stdout.flush().unwrap();
        let mut input = String::new();
        stdin.lock().read_line(&mut input).unwrap();

        if input.trim_start_matches(" ") == "\n" {
            continue;
        }

        if input.trim() == ":quit" || input.trim() == ":q" {
            println!("See ya!");
            return ();
        }

        if let Some(input) = input.trim_start().strip_prefix(":debug") {
            let mut parser = Parser::new(&input.trim_start());
            match parser.parse_expr() {
                Ok(expr) => {
                    let mut current = expr.clone();
                    loop {
                        println!("#> {}", current);
                        let next = current.eval_one(&state.bindings);
                        if next == current {
                            break;
                        }
                        print!("-->");
                        stdout.flush().unwrap();

                        let mut input = String::new();
                        stdin.lock().read_line(&mut input).unwrap();
                        if input.trim() == "quit" || input.trim() == "q" {
                            println!("!> Quit debugging!");
                            break;
                        }

                        current = next;
                    }
                    continue;
                }
                Err(e) => println!("!> {}", e),
            }
        }

        if let Some(input) = input.trim().strip_prefix(":ast") {
            let mut parser = Parser::new(&input.trim_start());
            match parser.parse_expr() {
                Ok(expr) => println!("{}", expr.display_ast()),
                Err(e) => println!("!> {}", e),
            }
            continue;
        }

        if let Some(input) = input.trim().strip_prefix(":let") {
            let mut parser = Parser::new(&input.trim_start());
            match parser.parse_binding() {
                Ok(binding) => {
                    println!("!> Created binding '{}'", binding.name);
                    state.push_binding(binding);
                }
                Err(e) => println!("!> {}", e),
            }
            continue;
        }

        if let Some(..) = input.trim().strip_prefix(":list") {
            println!("{}", state.display_bindigs());
            continue;
        }

        let mut parser = Parser::new(&input);
        match parser.parse_expr() {
            Ok(expr) => println!("λ> {}", expr.eval(&state.bindings)),
            Err(e) => println!("!> {}", e),
        };
    }
}
