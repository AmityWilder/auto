use crate::lang::{compiler::parse::ParseErrorType, types::Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    /// `(?<=\.)[\S]+(?=:)` at start of line
    Label,
    /// `(?<=\.)[\S]+` not at start of line
    LabelRef,
    /// `[\S]+` at start of line, after optional label
    Instr,
    /// `[a-z_][a-zA-Z0-9_]+`
    Name,
    /// starts with `"`, ends with un-escaped `"`
    String,
    /// `[A-Z]\w+`
    Enum,
    /// `\-?[0-9]+`
    Number,
    /// `#[0-9a-fA-F]{3}(?:[0-9a-fA-F]{3})?`
    Color,
    /// `&`
    Amp,
    /// `*`
    Star,
}

impl TokenType {
    pub fn categorize(s: &str) -> Option<Self> {
        s.chars().next().and_then(|ch| match ch {
            '.' => Some(Self::LabelRef),
            '_' | 'a'..='z' => Some(Self::Name),
            '"' => Some(Self::String),
            'A'..='Z' => Some(Self::Enum),
            '-' | '0'..='9' => Some(Self::Number),
            '#' => Some(Self::Color),
            '&' => Some(Self::Amp),
            '*' => Some(Self::Star),
            _ => None,
        })
    }

    pub fn split_off<'a>(self, s: &mut &'a str) -> Result<&'a str, ParseErrorType> {
        let mid = match self {
            Self::Label => s
                .find(':')
                .ok_or_else(|| ParseErrorType::MalformedLabel(s.to_string()))?,
            Self::Instr | Self::Name | Self::Enum | Self::Number | Self::LabelRef => {
                s.find(char::is_whitespace).unwrap_or(s.len())
            }
            Self::String => {
                let mut is_escaped = true;
                s.find(|ch| {
                    if !is_escaped {
                        if ch == '\\' {
                            is_escaped = true;
                        } else if ch == '"' {
                            return true;
                        }
                    } else {
                        is_escaped = false;
                    }
                    false
                })
                .inspect(|&mid| debug_assert!(s[..mid].ends_with('"')))
                .ok_or_else(|| ParseErrorType::UnclosedString(s.to_string()))?
            }
            Self::Color => "#FFFFFF".len(),
            Self::Amp => '&'.len_utf8(),
            Self::Star => '*'.len_utf8(),
        };
        let (front, back) = s.split_at(mid);
        *s = back;
        Ok(front)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, s: &'a str) -> Result<Self, ParseErrorType> {
        debug_assert_eq!(TokenType::categorize(s), Some(ty));
        match ty {
            TokenType::Label if s.contains(char::is_whitespace) => {
                Err(ParseErrorType::MalformedLabel(s.to_string()))
            }
            TokenType::Number if s.parse::<i32>().is_err() => {
                Err(ParseErrorType::InvalidLiteral(Type::Int, s.to_string()))
            }
            TokenType::Color
                if s['#'.len_utf8()..].contains(|ch: char| !ch.is_ascii_hexdigit()) =>
            {
                Err(ParseErrorType::InvalidLiteral(Type::Color, s.to_string()))
            }
            _ => Ok(Self { ty, text: s }),
        }
    }
}

#[derive(Debug, Clone)]
struct Tokenize<'a> {
    s: &'a str,
}

impl<'a> Tokenize<'a> {
    fn new(s: &'a str) -> Self {
        Self { s }
    }
}

impl<'a> Iterator for Tokenize<'a> {
    type Item = Result<Token<'a>, ParseErrorType>;

    fn next(&mut self) -> Option<Self::Item> {
        TokenType::categorize(self.s)
            .map(|ty| ty.split_off(&mut self.s).and_then(|s| Token::new(ty, s)))
    }
}

#[derive(Debug, Clone)]
pub struct TokenizedLine<'a> {
    pub row: usize,
    pub text: &'a str,
    pub tokens: Box<[Token<'a>]>,
}

impl<'a> TokenizedLine<'a> {
    pub fn lex_line(row: usize, mut s: &'a str) -> Result<Self, ParseErrorType> {
        Ok(Self {
            row,
            text: s,
            tokens: {
                if let Some((pre_comment, _)) = s.split_once("//") {
                    s = pre_comment.trim();
                }

                let label = if let Some(after_dot) = s.strip_prefix('.') {
                    let (lbl, after_label) = after_dot
                        .split_once(':')
                        .filter(|l| !l.0.contains(char::is_whitespace))
                        .ok_or_else(|| ParseErrorType::MalformedLabel(s.to_string()))?;
                    s = after_label;
                    Some(Ok(Token {
                        ty: TokenType::Label,
                        text: lbl,
                    }))
                } else {
                    None
                };

                let instruction = if let Some(ins) = s.split_whitespace().next() {
                    s = s[ins.len()..].trim_start();
                    Some(Ok(Token {
                        ty: TokenType::Instr,
                        text: ins,
                    }))
                } else {
                    None
                };

                label
                    .into_iter()
                    .chain(instruction.into_iter())
                    .chain(Tokenize::new(s))
                    .collect::<Result<Vec<_>, ParseErrorType>>()?
                    .into_boxed_slice()
            },
        })

        // let (mut code, comment) = s.split_at(s.find("//").unwrap_or(s.len()));
        // if let Some(after_dot) = code.strip_prefix('.') {
        //     (*label, code) = after_dot
        //         .split_once(':')
        //         .map(|(a, b)| (Some(a), b))
        //         .ok_or_else(|| ParseErrorType::MalformedLabel(s.to_string()))?;
        // }
        // let mut it = std::iter::from_fn(|| {
        //     let mid;
        //     if code.starts_with('"') {
        //         let mut is_escaped = false;
        //         'find_mid: {
        //             for (i, ch) in code.char_indices().skip(1) {
        //                 if !is_escaped {
        //                     if ch == '\\' {
        //                         is_escaped = true;
        //                     } else if ch == '"' {
        //                         mid = i + const { '"'.len_utf8() };
        //                         break 'find_mid;
        //                     }
        //                 } else {
        //                     is_escaped = false;
        //                 }
        //             }
        //             mid = code.len();
        //         }
        //     } else {
        //         mid = code.find(char::is_whitespace).unwrap_or(code.len());
        //     }
        //     (mid > 0).then(|| {
        //         let (start, rest) = code.split_at(mid);
        //         code = rest.trim_start();
        //         start
        //     })
        // });
        // *ins = it.next();
        // args.clear();
        // args.extend(it);
        // if false {
        //     print!(
        //         "{:>4} |     {}",
        //         row + 1,
        //         ins.unwrap_or_default().yellow(),
        //     );
        //     for arg in args {
        //         if arg.starts_with(|ch: char| ch == '_' || ch.is_ascii_lowercase()) {
        //             print!(" {}", arg.purple());
        //         } else {
        //             print!(" {}", arg.blue());
        //         }
        //     }
        //     println!(" {}", comment.green());
        // }
        // Ok(())
    }
}
