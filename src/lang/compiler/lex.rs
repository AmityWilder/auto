use crate::lang::compiler::parse::ParseErrorType;
use colored::Colorize;

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

pub struct Token<'a> {
    ty: TokenType,
    text: &'a str,
}

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
                    .chain(std::iter::from_fn(|| {
                        let mid = if s.starts_with('"') {
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
                            .ok_or_else(|| ParseErrorType::UnclosedString(s.to_string()))
                            .map(|mid| (TokenType::String, mid))
                        } else {
                            if s.starts_with('&') {
                                Ok((TokenType::Amp, '&'.len_utf8()))
                            } else if s.starts_with('*') {
                                Ok((TokenType::Star, '*'.len_utf8()))
                            } else if s.starts_with('#') {
                                todo!()
                            } else {
                                Ok((TokenType::Name, s.find(char::is_whitespace).unwrap_or(s.len())))
                            }
                        };
                        match mid {
                            Ok((_, 0)) => None,
                            Err(e) => Some(Err(e)),
                            Ok(mid @ 1..) => {
                                let (tkn, rest) = s.split_at(mid);
                                s = rest.trim_start();
                                Some(Ok(Token {
                                    ty: if tkn == "&" {
                                        TokenType::Amp
                                    } else if tkn == "*" {
                                        TokenType::Star
                                    } else if tkn.starts_with('#') {
                                        TokenType::Color
                                    } else if tkn.starts_with(|ch: char| ch == '-' || ch.is_ascii_digit()),
                                    text: tkn,
                                }))
                            }
                        }
                    }))
                    .collect::<Result<Vec<_>, ParseErrorType>>()
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
