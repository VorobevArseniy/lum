use std::{
    collections::HashMap,
    fmt::{Display, Write},
    fs,
    hash::Hash,
    io::{self, BufRead, Stdin, Stdout, Write as IOWrite},
    str::Chars,
};

// Lexer ------------------

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Hash)]
pub enum Token {
    Lambda,    // \ or λ
    Dot,       // .
    LParen,    // (
    RParen,    // )
    Equal,     // =
    SemiColon, // ;
    Comment,   // --

    Ident(String),

    // special
    Invalid(char),
    Newline,
    EOI,
}

#[derive(Debug)]
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
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn drop_line(&mut self) {
        while let Some(c) = self.cur {
            self.advance();
            if c == '\n' {
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

        loop {
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
                    ';' => {
                        self.advance();
                        Token::SemiColon
                    }
                    '-' => {
                        self.advance();
                        if self.cur == Some('-') {
                            self.advance();
                            self.drop_line();
                            continue;
                        } else {
                            Token::Invalid(c)
                        }
                    }
                    '\n' => {
                        self.advance();
                        Token::Newline
                    }
                    c if c.is_alphanumeric() => Token::Ident(self.read_ident()),
                    c => {
                        self.advance();
                        Token::Invalid(c)
                    }
                },
                None => Token::EOI,
            };

            return token;
        }
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
                "Error: Unexpected identifier while parsing: '{:?}'",
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
                "Error: Unexpected token '{:?}'. Expected a primary expression instead",
                self.cur
            )),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        while self.cur != Token::EOI && self.cur != Token::RParen && self.cur != Token::SemiColon {
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

    fn parse_file(&mut self) -> Result<Vec<Binding>, String> {
        let mut res = vec![];

        while self.cur != Token::EOI {
            let binding = self.parse_binding()?;
            res.push(binding);
            self.expect(Token::SemiColon)?;
        }

        Ok(res)
    }
}

// -----------------------

// Evaluation ------------

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var { name: String },
    Fun { arg: String, body: Box<Expr> },
    App { lhs: Box<Expr>, rhs: Box<Expr> },
}

impl Expr {
    fn var(name: impl Into<String> + Clone + Hash) -> Self {
        Self::Var { name: name.into() }
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

    fn eval_one(&self, bindings: &HashMap<String, Binding>) -> Self {
        match self.clone() {
            Expr::Var { name, .. } => {
                if let Some(binding) = bindings.get(&name) {
                    binding.body.clone()
                } else {
                    self.clone()
                }
            }
            Expr::Fun { arg, body } => {
                // preventing binding capture
                let filtered_bindings = bindings
                    .clone()
                    .into_iter()
                    .filter(|(name, _)| **name != arg)
                    .collect();

                let evaluated_body = body.eval_one(&filtered_bindings);

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

                let evaluated_lhs = lhs.eval_one(bindings);
                if *lhs != evaluated_lhs {
                    return Expr::app(evaluated_lhs, *rhs.clone());
                }

                let evaluated_rhs = rhs.eval_one(bindings);
                if *rhs != evaluated_rhs {
                    return Expr::app(evaluated_lhs, evaluated_rhs);
                }

                self.clone()
            }
        }
    }

    fn eval(&self, bindings: &HashMap<String, Binding>) -> Expr {
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

#[derive(Debug, Clone, Copy)]
enum CommandKind {
    Quit,
    AST,
    Debug,
    Let,
    List,
    Delete,
    Load,
    Save,
    Help,

    Eval,
    Unknown,
}

impl CommandKind {
    fn match_input(input: &str) -> (Self, &str) {
        let trimmed = input.trim();

        if let Some(cmd) = input.strip_prefix(":") {
            let (cmd, args) = match cmd.find(' ') {
                Some(pos) => (&cmd[0..pos], &cmd[pos..]),
                None => (cmd, ""),
            };

            let args = args.trim();

            match cmd {
                "quit" | "q" => (Self::Quit, args),
                "debug" | "d" => (Self::Debug, args),
                "help" | "h" => (Self::Help, args),
                "ast" => (Self::AST, args),
                "let" => (Self::Let, args),
                "load" => (Self::Load, args),
                "save" => (Self::Save, args),
                "delete" => (Self::Delete, args),
                "list" => (Self::List, args),
                _ => (Self::Unknown, cmd),
            }
        } else {
            (Self::Eval, trimmed)
        }
    }
}

struct Command {
    kind: CommandKind,
    name: String,
    alias: Option<String>,
    sig: String,
    short_desc: String,
}

impl Command {
    fn new(
        kind: CommandKind,
        name: impl Into<String>,
        alias: Option<String>,
        sig: impl Into<String>,
        short_desc: impl Into<String>,
    ) -> Self {
        Self {
            kind,
            name: name.into(),
            alias,
            sig: sig.into(),
            short_desc: short_desc.into(),
        }
    }
}

struct State {
    bindings: HashMap<String, Binding>,
    commands: Vec<Command>,
    stdin: Stdin,
    stdout: Stdout,
}

impl State {
    fn new(stdin: Stdin, stdout: Stdout) -> Self {
        let commands = vec![
            Command::new(
                CommandKind::Debug,
                "debug",
                Some(":d".to_string()),
                "<expr>",
                "step-by-step evaluation of the expression",
            ),
            Command::new(
                CommandKind::Let,
                "let",
                None,
                "[name] = <expr>",
                "save expression in a associative name (binding)",
            ),
        ];

        Self {
            bindings: HashMap::new(),
            commands,
            stdin,
            stdout,
        }
    }

    fn read_bindings_from_file(&mut self, path: &str) {
        match fs::read_to_string(path) {
            Ok(input) => {
                let mut parser = Parser::new(&input);
                match parser.parse_file() {
                    Ok(bindings) => bindings
                        .iter()
                        .for_each(|binding| self.add_binding(binding.clone())),
                    Err(e) => println!("!> Error parsing file '{}': {}", path, e),
                }
            }
            Err(e) => println!("!> Error reading file '{}': {}", path, e),
        }
    }

    fn add_binding(&mut self, binding: Binding) {
        println!("!> Created binding '{}'", binding.name);
        self.bindings.insert(binding.name.clone(), binding);
    }

    fn delete_binding(&mut self, name: &String) -> Result<String, String> {
        match self.bindings.remove(name) {
            Some(binding) => Ok(binding.name),
            None => Err(format!("binding '{}' was not found", name)),
        }
    }

    fn display_bindigs(&self) -> String {
        self.bindings
            .values()
            .map(|binding| format!(" > {};\n", binding))
            .collect()
    }

    fn save_bindings_to_file(&self, path: &str) {
        let contents: String = self
            .bindings
            .iter()
            .map(|(_, binding)| format!("{};\n", binding))
            .collect();
        match fs::write(path, contents) {
            Ok(_) => println!("!> Saved all bindings to '{}'", path),
            Err(e) => println!("!> Error saving bindings to '{}': {}", path, e),
        }
    }

    fn debug_command_handler(&mut self, input: &str) {
        let mut parser = Parser::new(&input.trim_start());
        match parser.parse_expr() {
            Ok(expr) => {
                println!(
                    "!> Debugging {}\n > Press Enter to continue evaluation or type 'quit' to exit debugging",
                    expr
                );
                let mut current = expr.clone();
                loop {
                    println!("#> {}", current);
                    let next = current.eval_one(&self.bindings);
                    if next == current {
                        break;
                    }
                    print!("-->");
                    self.stdout.flush().unwrap();

                    let mut input = String::new();
                    self.stdin.lock().read_line(&mut input).unwrap();
                    if input.trim() == "quit" || input.trim() == "q" {
                        println!("!> Quit debugging!");
                        break;
                    }

                    current = next;
                }
            }
            Err(e) => println!("!> {}", e),
        }
    }

    fn ast_command_handler(&self, input: &str) {
        let mut parser = Parser::new(&input.trim());
        match parser.parse_expr() {
            // removing last newline
            // TODO find better solution
            Ok(expr) => println!("{}", expr.display_ast().strip_suffix("\n").unwrap()),
            Err(e) => println!("!> {}", e),
        }
    }

    fn let_command_handler(&mut self, input: &str) {
        let mut parser = Parser::new(&input.trim());
        match parser.parse_binding() {
            Ok(binding) => self.add_binding(binding),
            Err(e) => println!("!> {}", e),
        }
    }

    fn load_command_handler(&mut self, input: &str) {
        self.read_bindings_from_file(&input.trim());
    }

    fn save_command_handler(&mut self, input: &str) {
        self.save_bindings_to_file(&input.trim());
    }

    fn delete_command_handler(&mut self, input: &str) {
        let mut parser = Parser::new(&input.trim());
        match parser.expect_ident() {
            Ok(name) => match self.delete_binding(&name) {
                Ok(name) => println!("!> Removed binding '{}'", name),
                Err(e) => println!("!> Error: {}", e),
            },
            Err(e) => println!("!> {}", e),
        }
    }

    fn list_command_handler(&self) {
        println!("{}", self.display_bindigs());
    }

    fn eval_handler(&self, input: &str) {
        let mut parser = Parser::new(&input);
        match parser.parse_expr() {
            Ok(expr) => println!("λ> {}", expr.eval(&self.bindings)),
            Err(e) => println!("!> {}", e),
        };
    }

    fn help_handler(&self) {
        let max_name_width = self
            .commands
            .iter()
            .map(|cmd| cmd.name.len())
            .max()
            .unwrap_or(0);
        let max_sig_width = self
            .commands
            .iter()
            .map(|cmd| cmd.sig.len())
            .max()
            .unwrap_or(0);

        self.commands.iter().for_each(|cmd| {
            println!(
                ">  :{name:<width$} {sig:<sig_width$} - {desc}",
                name = cmd.name,
                width = max_name_width,
                sig = cmd.sig,
                sig_width = max_sig_width,
                desc = cmd.short_desc
            );
        })
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub body: Expr,
}

impl Binding {
    fn new(name: String, body: Expr) -> Self {
        Self { name, body }
    }
}

fn main() {
    let stdin = io::stdin();
    let stdout = io::stdout();

    let mut state = State::new(stdin, stdout);

    loop {
        print!("λ> ");
        state.stdout.flush().unwrap();

        let mut input = String::new();
        state.stdin.read_line(&mut input).unwrap();
        let trimmed = input.trim();

        if trimmed.is_empty() {
            continue;
        }

        let (command, arg) = CommandKind::match_input(trimmed);

        match command {
            CommandKind::Quit => {
                println!("See ya!");
                return ();
            }
            CommandKind::Debug => state.debug_command_handler(arg),
            CommandKind::List => state.list_command_handler(),
            CommandKind::Delete => state.delete_command_handler(arg),
            CommandKind::Save => state.save_command_handler(arg),
            CommandKind::Load => state.load_command_handler(arg),
            CommandKind::Let => state.let_command_handler(arg),
            CommandKind::AST => state.ast_command_handler(arg),
            CommandKind::Help => state.help_handler(),

            CommandKind::Unknown => {
                state.help_handler();
                println!("!> Error: Unknown command")
            }
            CommandKind::Eval => state.eval_handler(trimmed),
        }
    }
}
