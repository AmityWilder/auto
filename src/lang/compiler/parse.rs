use colored::Colorize;
use std::collections::HashMap;

use crate::lang::{
    address::{Address, AddressRange, UAddr},
    compiler::{Program, lex},
    instructions::{ArgDirection, Instruction},
    memory::Source,
    types::Type,
};

#[derive(Debug)]
pub enum ParseErrorType {
    UnknownInstruction(String),
    MalformedLabel(String),
    UnclosedString(String),
    UnknownVariable(String),
    InvalidLiteral(Type, String),
    UnknownImmType(String),
    TypeDeductionFailed(&'static str),
    ArgCountMismatch {
        instruction: &'static str,
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
                instruction: ins,
                expect,
                actual,
            } => write!(
                f,
                "`{ins}` takes {} arguments but {actual} were supplied",
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
        let mut args = Vec::new();
        let mut label = None;
        let exec_lines = s
            .lines()
            .enumerate()
            .filter_map(
                |(n, line)| -> Option<Result<(Option<&str>, Instruction), ParseError>> {
                    let mut ins = None;
                    if let Err(e) = TokenizedLine::lex_line(
                        n, line, &mut label, &mut ins, &mut args,
                    )
                    .map_err(|ty| ParseError {
                        ty,
                        line: n,
                        code: line.to_string(),
                    }) {
                        return Some(Err(e));
                    };
                    ins.map(|ins| {
                        Instruction::from_str(&mut stack, ins, &args)
                            .inspect(|instruction| println!("parsed: {instruction}"))
                            .map_err(|ty| ParseError {
                                ty,
                                line: n,
                                code: line.to_string(),
                            })
                            .map(|x| (label.take(), x))
                    })
                },
            )
            .collect::<Result<Vec<(Option<&str>, Instruction)>, ParseError>>()?;

        let label_map = exec_lines
            .iter()
            .enumerate()
            .filter_map(|(n, (l, _))| l.map(|l| (l.to_string(), Address(n as UAddr))))
            .collect::<HashMap<String, Address>>();

        let code = exec_lines
            .into_iter()
            .map(|(_, mut c)| {
                fn label_bytes_to_address(
                    label_map: &HashMap<String, Address>,
                    bytes: &[u8],
                ) -> Box<[u8]> {
                    Box::from(
                        label_map
                            .get(str::from_utf8(bytes).unwrap())
                            .unwrap()
                            .0
                            .to_ne_bytes(),
                    )
                }
                match &mut c {
                    Instruction::Set {
                        T: Type::Label,
                        dest: _,
                        src: Source::Immediate(label),
                    }
                    | Instruction::Deref {
                        T: Type::Label,
                        dest: _,
                        src: Source::Immediate(label),
                    }
                    | Instruction::Eq {
                        T: Type::Label,
                        dest: _,
                        lhs: Source::Immediate(label),
                        rhs: Source::Address(_),
                    }
                    | Instruction::Eq {
                        T: Type::Label,
                        dest: _,
                        lhs: Source::Address(_),
                        rhs: Source::Immediate(label),
                    }
                    | Instruction::Goto {
                        label: Source::Immediate(label),
                    } => *label = label_bytes_to_address(&label_map, label),

                    Instruction::Eq {
                        T: Type::Label,
                        dest: _,
                        lhs: Source::Immediate(label1),
                        rhs: Source::Immediate(label2),
                    } => {
                        *label1 = label_bytes_to_address(&label_map, label1);
                        *label2 = label_bytes_to_address(&label_map, label2);
                    }

                    Instruction::Print { what } => {
                        for label in what.iter_mut().filter_map(|x| match x {
                            (Type::Label, Source::Immediate(label)) => Some(label),
                            _ => None,
                        }) {
                            *label = label_bytes_to_address(&label_map, label);
                        }
                    }
                    _ => {}
                }
                c
            })
            .collect();

        // for line in exec_lines {
        //     if
        // }

        Ok(Program(code))
    }
}
