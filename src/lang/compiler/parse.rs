use crate::{
    input::{KeyboardKey, MouseButton},
    lang::{
        address::{Address, AddressRange, UAddr},
        compiler::{
            Program,
            lex::{Token, TokenType, TokenizedLine},
        },
        instructions::{ArgDirection, Instruction, InstructionId},
        memory::Source,
        types::Type,
    },
    screen::ColorRGB,
};
use enigo::{Coordinate, Direction};
use std::{collections::HashMap, num::NonZeroUsize};

#[derive(Debug)]
pub enum ParseErrorType {
    UnknownInstruction(String),
    MalformedLabel(String),
    ImmediateReference,
    DuplicateLabel(String),
    UnclosedString(String),
    UnknownVariable(String),
    InvalidLiteral(Type, String),
    UnknownImmType(String),
    TypeDeductionFailed(&'static str),
    ArgCountMismatch {
        instruction: InstructionId,
        expect: &'static [usize],
        actual: usize,
    },
    TypeMismatch {
        arg: String,
        expect: Type,
        actual: Type,
    },
    OutOfMemory {
        requested: Option<UAddr>,
    },
}

#[derive(Debug)]
pub struct ParseError {
    ty: ParseErrorType,
    line: usize,
    code: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseErrorType::*;
        write!(f, "at line {}: ", self.line + 1)?;
        match &self.ty {
            UnknownInstruction(name) => write!(f, "unknown instruction: `{name}`"),
            MalformedLabel(label) => write!(f, "label `{label}` is missing a colon"),
            ImmediateReference => write!(f, "cannot reference an immediate value/reference"),
            DuplicateLabel(label) => write!(f, "label `{label}` appears multiple times"),
            UnclosedString(s) => write!(f, "string argument is mising a closing '\"': {s}"),
            UnknownVariable(name) => write!(f, "unknown variable: `{name}`"),
            InvalidLiteral(ty, value) => {
                write!(f, "could not parse {value:?} as a {ty:?} literal")
            }
            UnknownImmType(value) => {
                write!(
                    f,
                    "cannot deduce type associated with the literal {value:?}"
                )
            }
            TypeDeductionFailed(id) => {
                write!(f, "could not deduce the type of generic argument `{id}`")
            }
            ArgCountMismatch {
                instruction,
                expect,
                actual,
            } => write!(
                f,
                "`{instruction}` takes {} arguments but {actual} were supplied",
                match expect {
                    [] => unimplemented!(),
                    [n] => format!("{n}"),
                    [n, m] => format!("{n} or {m}"),
                    [rest @ .., last] => format!(
                        "{}or {last}",
                        rest.iter().map(|n| format!("{n}, ")).collect::<String>()
                    ),
                }
            ),
            TypeMismatch {
                arg,
                expect,
                actual,
            } => {
                write!(
                    f,
                    "type mismatch: `{arg}` is {actual:?}, expected {expect:?}"
                )
            }
            OutOfMemory {
                requested: Some(requested),
            } => write!(f, "out of memory. requested bytes: {requested}"),
            OutOfMemory { requested: None } => {
                write!(f, "out of memory. requested bytes: >{}", UAddr::MAX)
            }
        }?;
        write!(f, "\n {} |     {}", self.line + 1, self.code)
    }
}

impl std::error::Error for ParseError {}

pub struct VarTable {
    vars: HashMap<String, (Type, AddressRange)>,
    available: AddressRange,
}

impl VarTable {
    pub fn new(stack_size: UAddr) -> Self {
        // SAFETY: No address can be smaller than 0
        let full_range = AddressRange::range_to(Address(stack_size));
        Self {
            vars: HashMap::new(),
            available: full_range,
        }
    }

    #[inline]
    pub fn get(&self, var: &str) -> Option<&(Type, AddressRange)> {
        self.vars.get(var)
    }

    #[inline]
    pub fn get_or_err(&self, var: &str) -> Result<&(Type, AddressRange), ParseErrorType> {
        self.get(var)
            .ok_or_else(|| ParseErrorType::UnknownVariable(var.to_string()))
    }

    /// - [`None`]: type is unknown/dependent but not necessarily invalid
    /// - [`Err`]: type is invalid
    pub fn typecheck(
        &self,
        direction: ArgDirection,
        arg: &str,
    ) -> Option<Result<Type, ParseErrorType>> {
        use ArgDirection::*;
        match direction {
            // Type can be deduced
            In => {
                if arg.starts_with(|ch: char| ch == '_' || ch.is_ascii_lowercase()) {
                    self.typecheck(InOut, arg)
                } else {
                    Some(
                        Type::try_deduce_imm(arg, self)
                            .ok_or_else(|| ParseErrorType::UnknownImmType(arg.to_string())),
                    )
                }
            }

            // Type can only be deduced if initialized
            InOut => self.vars.get(arg).map(|(ty, _)| ty).cloned().map(Ok),

            // Type cannot be deduced
            Out => None,
        }
    }

    /// Tries to identify a concrete type that all provided arguments CAN be while
    /// meeting the constraint, preferring items earlier in the array.
    ///
    /// Returns [`None`] if no shared type can be found that meets the constraints.
    pub fn deduce<'a, I, F>(
        &self,
        generic: &'static str,
        args: I,
        constraint: F,
    ) -> Result<Type, ParseErrorType>
    where
        I: IntoIterator<
            Item = (
                ArgDirection,
                &'a str,
                Option<Box<dyn FnOnce(Type) -> Option<Result<Type, ParseErrorType>>>>,
            ),
        >,
        F: FnOnce(&Type) -> bool,
    {
        let mut it = args.into_iter().flat_map(|(direction, arg, f)| {
            match (self.typecheck(direction, arg), f) {
                (Some(Ok(t)), Some(f)) => f(t),
                (x, _) => x,
            }
        });

        it.next()
            .and_then(Result::ok)
            .filter(|t0| it.all(|t| t.is_ok_and(|t| &t == t0)))
            .filter(constraint)
            .ok_or(ParseErrorType::TypeDeductionFailed(generic))
    }

    pub fn lookup_out(
        &mut self,
        direction: ArgDirection,
        arg: &str,
        ty: &Type,
    ) -> Result<AddressRange, ParseErrorType> {
        use ArgDirection::*;
        match direction {
            In => unimplemented!(),

            InOut => {
                let (var_ty, addr) = self.get_or_err(arg)?;

                if var_ty == ty {
                    Ok(*addr)
                } else {
                    Err(ParseErrorType::TypeMismatch {
                        arg: arg.to_string(),
                        expect: ty.clone(),
                        actual: var_ty.clone(),
                    })
                }
            }

            Out => {
                if self.vars.get(arg).is_none_or(|(var_ty, _)| var_ty != ty) {
                    let size = ty
                        .size()
                        .map_err(|_| ParseErrorType::OutOfMemory { requested: None })?;
                    let (addr, available) =
                        self.available
                            .split_at(size)
                            .ok_or(ParseErrorType::OutOfMemory {
                                requested: Some(size),
                            })?;
                    self.available = available;
                    self.vars.insert(arg.to_string(), (ty.clone(), addr));
                }
                self.lookup_out(InOut, arg, ty)
            }
        }
    }

    pub fn lookup(
        &mut self,
        direction: ArgDirection,
        arg: &str,
        ty: &Type,
    ) -> Result<Source, ParseErrorType> {
        use ArgDirection::*;
        match direction {
            In => {
                if arg.starts_with(|ch: char| ch == '_' || ch.is_ascii_lowercase()) {
                    self.lookup_out(InOut, arg, ty).map(Source::Address)
                } else {
                    ty.try_parse_imm(arg, self)
                        .map(Source::Immediate)
                        .ok_or_else(|| ParseErrorType::InvalidLiteral(ty.clone(), arg.to_string()))
                }
            }

            InOut | Out => self.lookup_out(direction, arg, ty).map(Source::Address),
        }
    }
}

impl std::str::FromStr for Program {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut stack = VarTable::new(4096);

        // --- pass 1: tokenize ---

        let document = s
            .lines()
            .enumerate()
            .map(|(n, line)| {
                TokenizedLine::lex_line(n, line).map_err(|ty| ParseError {
                    ty,
                    line: n,
                    code: line.to_string(),
                })
            })
            .filter(|line| !line.as_ref().is_ok_and(|x| x.tokens.is_empty()))
            .collect::<Result<Vec<TokenizedLine>, ParseError>>()?;

        // --- pass 2: give labels IDs ---

        type LabelId = usize;
        let mut lbl_id = 0;
        let mut lbl_lookup = HashMap::<String, LabelId>::new();

        for line in document.iter() {
            if let Some(Token {
                ty: TokenType::Label,
                text,
            }) = line.tokens.get(0)
            {
                if lbl_lookup.insert(text.to_string(), lbl_id).is_none() {
                    lbl_id += 1;
                } else {
                    return Err(ParseError {
                        ty: ParseErrorType::DuplicateLabel(text.to_string()),
                        line: line.row,
                        code: line.text.to_string(),
                    });
                }
            }
        }

        // --- pass 3: give shadowed variables unique IDs and replace labels with their IDs ---

        type VarId = usize;

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum Referencing {
            Direct,
            Ref,
            Derefs(NonZeroUsize),
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum Arg<'a> {
            Var { refr: Referencing, id: VarId },
            Label(LabelId),
            String(&'a str),
            Bool(bool, &'a str),
            Action(Direction, &'a str),
            Coordinate(Coordinate, &'a str),
            Button(MouseButton, &'a str),
            Key(KeyboardKey, &'a str),
            Number(i32, &'a str),
            Color(ColorRGB, &'a str),
        }

        impl Arg<'_> {
            fn get_type(&self) -> Option<Type> {
                match self {
                    Self::Var { refr, id } => todo!(),
                    Self::Label(_) => Some(Type::Label),
                    Self::String(_) => Some(Type::Str),
                    Self::Bool(_, _) => Some(Type::Bool),
                    Self::Action(direction, _) => Some(Type::Action),
                    Self::Coordinate(coordinate, _) => Some(Type::Coordinate),
                    Self::Button(button, _) => Some(Type::Button),
                    Self::Key(key, _) => Some(Type::Key),
                    Self::Number(_, _) => Some(Type::Int),
                    Self::Color(color_rgb, _) => Some(Type::Color),
                }
            }

            fn into_source(
                self,
                lbl_addresses: &[Address],
                var_addresses: &[AddressRange],
            ) -> Source {
                match self {
                    Self::Var { refr, id } => todo!(),
                    Self::Label(id) => {
                        Source::Immediate(Box::from(lbl_addresses[id].0.to_ne_bytes()))
                    }
                    Self::String(s) => Source::Immediate(Box::from(s.as_bytes())),
                    Self::Bool(value, _) => Source::Immediate(Box::from([value as u8])),
                    Self::Action(direction, _) => Source::Immediate(Box::from([direction as u8])),
                    Self::Coordinate(coordinate, _) => {
                        Source::Immediate(Box::from([coordinate as u8]))
                    }
                    Self::Button(button, _) => Source::Immediate(Box::from(button.to_ne_bytes())),
                    Self::Key(key, _) => Source::Immediate(Box::from(key.to_ne_bytes())),
                    Self::Number(int, _) => Source::Immediate(Box::from(int.to_ne_bytes())),
                    Self::Color(color, _) => Source::Immediate(Box::from(unsafe {
                        std::mem::transmute::<ColorRGB, [u8; size_of::<ColorRGB>()]>(color)
                    })),
                }
            }
        }

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum HirLine<'a> {
            Label {
                row: usize,
                label: LabelId,
            },
            Exec {
                row: usize,
                instruction: InstructionId,
                args: Box<[Arg<'a>]>,
            },
        }

        let mut var_lookup = HashMap::<String, VarId>::new();
        let mut var_types = Vec::<Option<Type>>::new();

        let hir = document
            .into_iter()
            .flat_map(|line| {
                let line_number = line.row;
                let line_text = line.text;
                let mut it = line.tokens.into_iter().peekable();

                let label = it
                    .next_if(|token| token.ty == TokenType::Label)
                    .map(|lbl| {
                        Ok(HirLine::Label {
                            row: line_number,
                            label: lbl_lookup
                                .get(lbl.text)
                                .copied()
                                .expect("all labels should have been discovered by label pass"),
                        })
                    })
                    .into_iter();

                let mut leading_amp = false;
                let mut stars: usize = 0;
                let ins = it
                    .next_if(|token| token.ty == TokenType::Instr)
                    .map(|ins| {
                        Ok(HirLine::Exec {
                            row: line_number,
                            instruction: ins.text.parse::<InstructionId>().map_err(|()| {
                                ParseError {
                                    ty: ParseErrorType::UnknownInstruction(ins.text.to_string()),
                                    line: line_number,
                                    code: line_text.to_string(),
                                }
                            })?,
                            args: it
                                .filter(|token| match token.ty {
                                    TokenType::Amp => {
                                        if leading_amp {
                                            stars = stars
                                                .checked_sub(1)
                                                .ok_or(ParseErrorType::ImmediateReference)
                                                .expect("todo: find a way to return this error");
                                        } else {
                                            leading_amp = true;
                                        }
                                        false
                                    }
                                    TokenType::Star => {
                                        stars += 1;
                                        false
                                    }
                                    _ => true,
                                })
                                .map(|token| {
                                    match token.ty {
                                        TokenType::LabelRef => lbl_lookup
                                            .get(token.text)
                                            .copied()
                                            .map(Arg::Label)
                                            .ok_or_else(|| {
                                                ParseErrorType::InvalidLiteral(
                                                    Type::Label,
                                                    token.text.to_string(),
                                                )
                                            }),

                                        TokenType::Name => {
                                            let id = match var_lookup.get(token.text).copied() {
                                                Some(id) => id,
                                                None => {
                                                    let id = var_types.len();
                                                    var_types.push(None);
                                                    _ = var_lookup
                                                        .insert(token.text.to_string(), id);
                                                    id
                                                }
                                            };
                                            Ok(Arg::Var {
                                                leading_amp,
                                                stars,
                                                id,
                                            })
                                        }

                                        TokenType::String => Ok(Arg::String(token.text)),
                                        TokenType::Enum => match token.text {
                                            "True" => Ok(Arg::Bool(true, token.text)),
                                            "False" => Ok(Arg::Bool(false, token.text)),
                                            "Press" => {
                                                Ok(Arg::Action(Direction::Press, token.text))
                                            }
                                            "Release" => {
                                                Ok(Arg::Action(Direction::Release, token.text))
                                            }
                                            "Click" => {
                                                Ok(Arg::Action(Direction::Click, token.text))
                                            }
                                            "Abs" => {
                                                Ok(Arg::Coordinate(Coordinate::Abs, token.text))
                                            }
                                            "Rel" => {
                                                Ok(Arg::Coordinate(Coordinate::Rel, token.text))
                                            }
                                            _ => {
                                                if let Ok(button) = token.text.parse() {
                                                    Ok(Arg::Button(button, token.text))
                                                } else if let Ok(key) = token.text.parse() {
                                                    Ok(Arg::Key(key, token.text))
                                                } else {
                                                    Err(ParseErrorType::UnknownImmType(
                                                        token.text.to_string(),
                                                    ))
                                                }
                                            }
                                        },
                                        TokenType::Number => token
                                            .text
                                            .parse()
                                            .map(|x| Arg::Number(x, token.text))
                                            .map_err(|_| {
                                                ParseErrorType::InvalidLiteral(
                                                    Type::Int,
                                                    token.text.to_string(),
                                                )
                                            }),
                                        TokenType::Color => token
                                            .text
                                            .parse()
                                            .map(|x| Arg::Color(x, token.text))
                                            .map_err(|_| {
                                                ParseErrorType::InvalidLiteral(
                                                    Type::Color,
                                                    token.text.to_string(),
                                                )
                                            }),

                                        TokenType::Label | TokenType::Instr => {
                                            unreachable!("can only be at the start of the line")
                                        }

                                        TokenType::Amp | TokenType::Star => {
                                            unreachable!("should have been filtered out")
                                        }
                                    }
                                    .map_err(|ty| ParseError {
                                        ty,
                                        line: line_number,
                                        code: line_text.to_string(),
                                    })
                                })
                                .collect::<Result<Box<[Arg]>, ParseError>>()?,
                        })
                    })
                    .into_iter();

                label.chain(ins)
            })
            .collect::<Result<Vec<HirLine>, ParseError>>()?
            .into_boxed_slice();

        // --- pass 4: Optimizations ---

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum MirLine<'a> {
            Initialize { var: VarId },
            Hir(HirLine<'a>),
        }

        let mir = hir
            .into_iter()
            .map(|line| Ok(MirLine::Hir(line))) // TODO: Add actual optimizations
            .collect::<Result<Vec<MirLine>, ParseError>>()?
            .into_boxed_slice();

        // --- pass 5: Compile into runnable instructions ---

        let var_types = var_types
            .into_iter()
            .map(|ty| ty.expect("all types should be known by this point"))
            .collect::<Vec<Type>>();

        let var_addresses = {
            let mut start = Address(0);
            var_types
                .iter()
                .map(|ty| {
                    let size = ty.size().map_err(|e| todo!("size error"))?;

                    let end = Address(start.0.wrapping_add(size));

                    AddressRange::new(start, end)
                        .inspect(|_| start = end)
                        .ok_or(ParseErrorType::OutOfMemory {
                            requested: Some(size),
                        })
                })
                .collect::<Result<Vec<AddressRange>, ParseErrorType>>()
                .map_err(|ty| ParseError {
                    ty,
                    line: 0,
                    code: "".to_string(), // hm...
                })?
        };

        #[allow(non_snake_case)]
        let code = mir
            .into_iter()
            .map(|line| {
                match line {
                    MirLine::Initialize { var } => todo!(),
                    MirLine::Hir(hir_line) => match hir_line {
                        HirLine::Label { row, label } => todo!(),
                        HirLine::Exec {
                            row,
                            instruction,
                            args,
                        } => match instruction {
                            InstructionId::Set => match &*args {
                                &[dest @ Arg::Var { id, .. }, src] => Ok(Instruction::Set {
                                    T: var_types[id],
                                    dest: var_addresses[id],
                                    src: src,
                                }),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Deref => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::If => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Goto => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Not => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Eq => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Add => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::GetPixel => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Print => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Wait => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::MoveMouse => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Key => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                            InstructionId::Button => match &*args {
                                &[] => todo!(),
                                _ => Err(ParseErrorType::ArgCountMismatch {
                                    instruction,
                                    expect: &[2],
                                    actual: args.len(),
                                }),
                            },
                        },
                    },
                }
                .map_err(|ty| ParseError {
                    ty,
                    line: todo!(),
                    code: todo!(),
                })
            })
            .collect::<Result<Vec<Instruction>, ParseError>>()?
            .into_boxed_slice();

        // let label_map = exec_lines
        //     .iter()
        //     .enumerate()
        //     .filter_map(|(n, (l, _))| l.map(|l| (l.to_string(), Address(n as UAddr))))
        //     .collect::<HashMap<String, Address>>();

        // let code = exec_lines
        //     .into_iter()
        //     .map(|(_, mut c)| {
        //         fn label_bytes_to_address(
        //             label_map: &HashMap<String, Address>,
        //             bytes: &[u8],
        //         ) -> Box<[u8]> {
        //             Box::from(
        //                 label_map
        //                     .get(str::from_utf8(bytes).unwrap())
        //                     .unwrap()
        //                     .0
        //                     .to_ne_bytes(),
        //             )
        //         }
        //         match &mut c {
        //             Instruction::Set {
        //                 T: Type::Label,
        //                 dest: _,
        //                 src: Source::Immediate(label),
        //             }
        //             | Instruction::Deref {
        //                 T: Type::Label,
        //                 dest: _,
        //                 src: Source::Immediate(label),
        //             }
        //             | Instruction::Eq {
        //                 T: Type::Label,
        //                 dest: _,
        //                 lhs: Source::Immediate(label),
        //                 rhs: Source::Address(_),
        //             }
        //             | Instruction::Eq {
        //                 T: Type::Label,
        //                 dest: _,
        //                 lhs: Source::Address(_),
        //                 rhs: Source::Immediate(label),
        //             }
        //             | Instruction::Goto {
        //                 label: Source::Immediate(label),
        //             } => *label = label_bytes_to_address(&label_map, label),

        //             Instruction::Eq {
        //                 T: Type::Label,
        //                 dest: _,
        //                 lhs: Source::Immediate(label1),
        //                 rhs: Source::Immediate(label2),
        //             } => {
        //                 *label1 = label_bytes_to_address(&label_map, label1);
        //                 *label2 = label_bytes_to_address(&label_map, label2);
        //             }

        //             Instruction::Print { what } => {
        //                 for label in what.iter_mut().filter_map(|x| match x {
        //                     (Type::Label, Source::Immediate(label)) => Some(label),
        //                     _ => None,
        //                 }) {
        //                     *label = label_bytes_to_address(&label_map, label);
        //                 }
        //             }
        //             _ => {}
        //         }
        //         c
        //     })
        //     .collect();

        // for line in exec_lines {
        //     if
        // }

        Ok(Program(code))
    }
}
