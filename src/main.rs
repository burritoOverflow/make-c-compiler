use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Read};
use std::process;

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    Identifier,
    Constant,
    KeywordInt,
    KeywordVoid,
    KeywordReturn,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    lexeme: String, // The actual string of characters that make up the token
    line: usize,    // Line number in the source code
    col: usize,     // Column number in the source code
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, line: usize, col: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            col,
        }
    }
}

// Define a custom error type for the lexer
#[derive(Debug)]
struct LexerError {
    message: String,
    line: usize,
    col: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Lexer Error at line {}, col {}: {}",
            self.line, self.col, self.message
        )
    }
}

impl std::error::Error for LexerError {} // Implement the standard Error trait

fn main() {
    let args: Vec<String> = env::args().collect();

    // For now: program --lex <filepath>
    if args.len() != 3 || args[1] != "--lex" {
        eprintln!("Usage: {} --lex <filepath>", args[0]);
        process::exit(1);
    }

    let filepath = &args[2];
    let source_code = match read_file(filepath) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filepath, e);
            process::exit(1);
        }
    };

    let tokens: Result<Vec<Token>, LexerError> = lexer(&source_code);
    if let Ok(token) = tokens {
        println!("{:?}", token);
    } else if let Err(e) = tokens {
        eprintln!("Lexer error: {}", e);
        process::exit(1);
    }
}

fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = fs::File::open(filepath)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

/*
Rough outline from book (page 9):

    while input isn't empty:
        if input starts with whitespace:
            trim whitespace from start of input
        else:
            find longest match at start of input for any regex in Table 1-1
            if no match is found, raise an error
            convert matching substring into a token
            remove matching substring from start of input
 */
fn lexer(source: &str) -> Result<Vec<Token>, LexerError> {
    // collect tokens
    let mut tokens = Vec::new();

    // allow for a peekable iterator over the characters in the source code (i.e "peek" at the next character without consuming it)
    let mut chars = source.chars().peekable();

    // track line and column numbers
    let mut line = 1;
    let mut col = 1;

    // while input isn't empty:
    while let Some(&c) = chars.peek() {
        match c {
            // Trim whitespace
            w if w.is_whitespace() => {
                chars.next();
                if w == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }
            // Valid symbols
            '(' => {
                tokens.push(Token::new(TokenType::OpenParen, c.to_string(), line, col));
                chars.next();
                col += 1;
            }
            ')' => {
                tokens.push(Token::new(TokenType::CloseParen, c.to_string(), line, col));
                chars.next();
                col += 1;
            }
            '{' => {
                tokens.push(Token::new(TokenType::OpenBrace, c.to_string(), line, col));
                chars.next();
                col += 1;
            }
            '}' => {
                tokens.push(Token::new(TokenType::CloseBrace, c.to_string(), line, col));
                chars.next();
                col += 1;
            }
            ';' => {
                tokens.push(Token::new(TokenType::Semicolon, c.to_string(), line, col));
                chars.next();
                col += 1;
            }
            // Identifiers and Keywords
            i if i.is_alphabetic() || i == '_' => {
                let start_col = col;
                let mut lexeme = String::new();

                // consume subsequent alphanumeric characters or underscores (valid identifier)
                while let Some(&p) = chars.peek() {
                    if p.is_alphanumeric() || p == '_' {
                        lexeme.push(p);
                        chars.next();
                        col += 1;
                    } else {
                        break;
                    }
                }

                let token_type = match lexeme.as_str() {
                    "int" => TokenType::KeywordInt,
                    "void" => TokenType::KeywordVoid,
                    "return" => TokenType::KeywordReturn,
                    _ => TokenType::Identifier,
                };
                tokens.push(Token::new(token_type, lexeme, line, start_col));
            }
            // Constants (Numeric Literals)
            d if d.is_digit(10) => {
                let start_col = col;
                let mut lexeme = String::new();

                while let Some(&p) = chars.peek() {
                    if p.is_digit(10) {
                        lexeme.push(p);
                        chars.next();
                        col += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::new(TokenType::Constant, lexeme, line, start_col));
            }
            // Unknown character results in an error
            _ => {
                chars.next(); // Consume the unknown character
                // don't bother incrementing col here, as we are returning an error
                return Err(LexerError {
                    message: format!("Unknown token '{}'", c),
                    line,
                    col,
                });
            }
        }
    }

    tokens.push(Token::new(TokenType::EOF, String::new(), line, col));
    Ok(tokens)
}
