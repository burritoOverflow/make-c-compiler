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

#[derive(Debug, Clone, PartialEq)]
struct Token {
    token_type: TokenType,
    lexeme: String, // The actual string of characters that make up the token
    line: usize,    // Line number in the source code, advanced with each newline
    col: usize,     // Column number in the source code, advanced with each character
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Special case for EOF which has no lexeme
        if self.token_type == TokenType::EOF {
            write!(f, "EOF at {}:{}", self.line, self.col)
        } else {
            write!(
                f,
                "{:?} \"{}\" at {}:{}",
                self.token_type, self.lexeme, self.line, self.col
            )
        }
    }
}

// Description of the error, along with the line and column where it occurred
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

impl std::error::Error for LexerError {}

fn main() {
    let args: Vec<String> = env::args().collect();
    /*
    TODO (from book): eventual opts are:

    --lex      Directs it to run the lexer, but stop before parsing
    --parse    Directs it to run the lexer and parser, but stop before assembly generation
    --codegen   Directs it to perform lexing, parsing, and assembly generation, but stop before code emission
    */

    // For now:, we've only implemented lexing so,
    // ./program --lex <filepath>
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
    match tokens {
        Ok(tokens) => {
            // Display the results of the lexing
            for token in tokens {
                println!("{}", token);
            }
        }
        // Failures are present when the lexer encounters an error (invalid token in input)
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = fs::File::open(filepath)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

/*
   Produces a list of tokens from the source code, provided as input
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
            '/' => {
                // Store starting position for reporting potential errors
                let start_line = line;
                let start_col = col;
                chars.next(); // Consume the first '/'
                col += 1;

                match chars.peek() {
                    // Single-line comment `//`
                    Some(&'/') => {
                        chars.next(); // Consume the second '/'
                        col += 1;
                        // Consume until newline or EOF
                        while let Some(&comment_char) = chars.peek() {
                            if comment_char == '\n' {
                                chars.next(); // Consume newline
                                line += 1;
                                col = 1;
                                break; // Exit comment loop
                            } else {
                                chars.next(); // Consume comment char
                                col += 1;
                            }
                        }
                        continue; // Skip to next token
                    }
                    // Multi-line comment `/*`
                    Some(&'*') => {
                        chars.next(); // Consume the '*'
                        col += 1;
                        let comment_start_line = start_line; // For error reporting
                        let comment_start_col = start_col;

                        loop {
                            match chars.next() {
                                Some('*') => {
                                    col += 1;
                                    // Check if the next char is '/' to close the comment
                                    if let Some(&'/') = chars.peek() {
                                        chars.next(); // Consume the '/'
                                        col += 1;
                                        break; // End of multi-line comment
                                    }
                                    // If it was just a '*', continue consuming
                                }
                                Some('\n') => {
                                    line += 1;
                                    col = 1;
                                }
                                Some(_) => {
                                    // Any other character inside the comment
                                    col += 1;
                                }
                                None => {
                                    // Reached EOF before finding '*/'
                                    return Err(LexerError {
                                        message: "Unterminated multi-line comment".to_string(),
                                        line: comment_start_line, // Report error at comment start
                                        col: comment_start_col,   // As above
                                    });
                                }
                            }
                        }
                        continue; // Skip to next token
                    }
                    _ => {
                        // It was just a single '/', which is an error in this grammar
                        return Err(LexerError {
                            message: "Unexpected character '/'".to_string(),
                            line: start_line,
                            col: start_col,
                        });
                    }
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

                // Consume all consecutive digits
                while let Some(&p) = chars.peek() {
                    if p.is_digit(10) {
                        lexeme.push(p);
                        chars.next();
                        col += 1;
                    } else {
                        break;
                    }
                }

                // Check if there are characters after the digits (invlid identifier)
                if let Some(&next_char) = chars.peek() {
                    if next_char.is_alphabetic() || next_char == '_' {
                        // Consume the invalid character to include it in the error context if needed,
                        // though the error message focuses on the invalid sequence starting with the number.
                        lexeme.push(next_char);
                        chars.next();
                        col += 1;

                        return Err(LexerError {
                            message: format!(
                                "Invalid numeric literal or identifier starting with digit: '{}'",
                                lexeme
                            ),
                            line,
                            col, // Report the location of the error as the start of this invalid identifier
                        });
                    }
                }

                // Otherwise, it's a valid numerical constant.
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /*
       Basic test cases for the lexer.
       Warning: keep in mind all the tests assert equality on the line and column numbers of the tokens.
    */
    #[test]
    fn test_lex_valid_basic() {
        let filepath = Path::new("./inputs/1.c");
        let source_code = match read_file(filepath.to_str().unwrap()) {
            Ok(content) => content,
            Err(e) => {
                panic!("Failed to read input file {:?}: {}", filepath, e);
            }
        };

        let result = lexer(&source_code);
        assert!(
            result.is_ok(),
            "Lexer failed on input file {:?}: {:?}",
            filepath,
            result.err()
        );

        let actual_tokens = result.unwrap();
        assert!(!actual_tokens.is_empty());

        let expected_tokens = vec![
            Token::new(TokenType::KeywordInt, "int".to_string(), 1, 1),
            Token::new(TokenType::Identifier, "main".to_string(), 1, 5),
            Token::new(TokenType::OpenParen, "(".to_string(), 1, 9),
            Token::new(TokenType::KeywordVoid, "void".to_string(), 1, 10),
            Token::new(TokenType::CloseParen, ")".to_string(), 1, 14),
            Token::new(TokenType::OpenBrace, "{".to_string(), 1, 16),
            Token::new(TokenType::KeywordReturn, "return".to_string(), 2, 5),
            Token::new(TokenType::Constant, "2".to_string(), 2, 12),
            Token::new(TokenType::Semicolon, ";".to_string(), 2, 13),
            Token::new(TokenType::CloseBrace, "}".to_string(), 3, 1),
            Token::new(TokenType::EOF, String::new(), 3, 2),
        ];

        assert_eq!(actual_tokens, expected_tokens, "Token sequence mismatch");
    }

    #[test]
    fn test_single_line_comment() {
        let filepath = Path::new("./inputs/singleLineComment.c");
        let source_code = match read_file(filepath.to_str().unwrap()) {
            Ok(content) => content,
            Err(e) => {
                panic!("Failed to read input file {:?}: {}", filepath, e);
            }
        };

        let result = lexer(&source_code);
        assert!(
            result.is_ok(),
            "Lexer failed on input file {:?}: {:?}",
            filepath,
            result.err()
        );

        let actual_tokens = result.unwrap();
        assert!(!actual_tokens.is_empty());

        let expected_tokens = vec![
            Token::new(TokenType::KeywordInt, "int".to_string(), 1, 1),
            Token::new(TokenType::Identifier, "main".to_string(), 1, 5),
            Token::new(TokenType::OpenParen, "(".to_string(), 1, 9),
            Token::new(TokenType::CloseParen, ")".to_string(), 1, 10),
            Token::new(TokenType::OpenBrace, "{".to_string(), 1, 12),
            Token::new(TokenType::KeywordInt, "int".to_string(), 3, 5),
            Token::new(TokenType::Identifier, "a".to_string(), 3, 9),
            Token::new(TokenType::Semicolon, ";".to_string(), 3, 10),
            Token::new(TokenType::KeywordReturn, "return".to_string(), 4, 5),
            Token::new(TokenType::Constant, "12".to_string(), 4, 12),
            Token::new(TokenType::Semicolon, ";".to_string(), 4, 14),
            Token::new(TokenType::CloseBrace, "}".to_string(), 5, 1),
            Token::new(TokenType::EOF, String::new(), 5, 2),
        ];

        assert_eq!(actual_tokens, expected_tokens, "Token sequence mismatch");
    }

    #[test]
    fn test_multi_line_comment() {
        let filepath = Path::new("./inputs/multiLineComment.c");
        let source_code = match read_file(filepath.to_str().unwrap()) {
            Ok(content) => content,
            Err(e) => {
                panic!("Failed to read input file {:?}: {}", filepath, e);
            }
        };

        let result = lexer(&source_code);
        assert!(
            result.is_ok(),
            "Lexer failed on input file {:?}: {:?}",
            filepath,
            result.err()
        );

        let actual_tokens = result.unwrap();
        assert!(!actual_tokens.is_empty());

        let expected_tokens = vec![
            Token::new(TokenType::KeywordInt, "int".to_string(), 1, 1),
            Token::new(TokenType::Identifier, "main".to_string(), 1, 5),
            Token::new(TokenType::OpenParen, "(".to_string(), 1, 9),
            Token::new(TokenType::KeywordVoid, "void".to_string(), 1, 10),
            Token::new(TokenType::CloseParen, ")".to_string(), 1, 14),
            Token::new(TokenType::OpenBrace, "{".to_string(), 1, 16),
            Token::new(TokenType::KeywordReturn, "return".to_string(), 6, 4),
            Token::new(TokenType::Constant, "313".to_string(), 6, 11),
            Token::new(TokenType::Semicolon, ";".to_string(), 6, 14),
            Token::new(TokenType::CloseBrace, "}".to_string(), 7, 1),
            Token::new(TokenType::EOF, String::new(), 7, 2),
        ];

        assert_eq!(actual_tokens, expected_tokens, "Token sequence mismatch");
    }
}
