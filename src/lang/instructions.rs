#![allow(non_snake_case)]

use crate::lang::{
    address::AddressRange,
    compiler::parse::{ParseErrorType, VarTable},
    memory::Source,
    types::Type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArgDirection {
    /// Can accept literals.
    /// Cannot create a new variable.
    /// Can be used to deduce generic types.
    ///
    /// ### Note
    ///
    /// In order to distinguish variable names from builtin enum variants,
    /// **variables must start with a lowercase ascii letter or underscore.**
    /// Variable names (as usual) cannot start with a number, but **unlike usual**,
    /// they *also* cannot start with capital letters.
    ///
    /// Any variable name starting with a capital letter is assumed to be a literal
    /// builtin enum variant, and will cause a parse error either because
    /// - constants cannot be assigned to, or
    /// - you said the name of a builtin enum variant that does not exist
    In,

    /// Cannot accept literals.
    /// Cannot create a new variable.
    /// Can be used to deduce generic types.
    InOut,

    /// Cannot accept literals.
    /// Can create a new variable.
    /// Cannot be used to deduce generic types.
    Out,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Set {
        T: Type,
        dest: AddressRange,
        src: Source,
    },
    Deref {
        T: Type,
        dest: AddressRange,
        src: Source,
    },
    If {
        cond: Source,
    },
    Goto {
        label: Source,
    },
    Eq {
        T: Type,
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    AddInts {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    AddColors {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    AddPtrInt {
        T: Type,
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    AddAssignInts {
        dest: AddressRange,
        rhs: Source,
    },
    AddAssignColors {
        dest: AddressRange,
        rhs: Source,
    },
    AddAssignPtrInt {
        T: Type,
        dest: AddressRange,
        rhs: Source,
    },
    GetPixel {
        dest: AddressRange,
        x: Source,
        y: Source,
    },
    Print {
        what: Box<[(Type, Source)]>,
    },
    Wait {
        ms: Source,
    },
    MoveMouse {
        coord: Source,
        x: Source,
        y: Source,
    },
    Key {
        action: Source,
        key: Source,
    },
    Button {
        action: Source,
        button: Source,
    },
}

macro_rules! tf {
    ($($pattern:pat => $out:expr),+ $(,)?) => {
        Some(Box::new(|ty| match ty { $($pattern => $out,)+ _ => None }) as Box<dyn FnOnce(Type) -> Option<Result<Type, ParseErrorType>>>)
    };
}

macro_rules! req {
    ($pattern:pat) => {
        |ty| matches!(ty, $pattern)
    };
}

fn any_type(_: &Type) -> bool {
    true
}

impl Instruction {
    pub(super) fn from_str<'a>(
        stack: &mut VarTable,
        ins: &'a str,
        args: &[&'a str],
    ) -> Result<Instruction, ParseErrorType> {
        use ArgDirection::*;

        match (ins, args) {
            ("set", &[dest, src]) => {
                let T = stack.deduce("T", [(Out, dest, None), (In, src, None)], any_type)?;
                let dest = stack.lookup_out(Out, dest, &T)?;
                let src = stack.lookup(In, src, &T)?;
                Ok(Self::Set { T, dest, src })
            }
            ("set", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "set",
                expect: &[2],
                actual: args.len(),
            }),
            ("if", &[cond]) => {
                let cond = stack.lookup(In, cond, &Type::Bool)?;
                Ok(Self::If { cond })
            }
            ("if", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "if",
                expect: &[1],
                actual: args.len(),
            }),
            ("goto", &[label]) => {
                let label = stack.lookup(In, label, &Type::Label)?;
                Ok(Self::Goto { label })
            }
            ("goto", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "goto",
                expect: &[1],
                actual: args.len(),
            }),
            ("eq", &[dest, lhs, rhs]) => {
                let T = stack.deduce("T", [(In, lhs, None), (In, rhs, None)], any_type)?;
                let dest = stack.lookup_out(Out, dest, &Type::Bool)?;
                let lhs = stack.lookup(In, lhs, &T)?;
                let rhs = stack.lookup(In, rhs, &T)?;
                Ok(Self::Eq { T, dest, lhs, rhs })
            }
            ("eq", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "eq",
                expect: &[3],
                actual: args.len(),
            }),
            ("*", &[dest, src]) => {
                let T = stack.deduce(
                    "T",
                    [
                        (Out, dest, None),
                        (In, src, tf!(Type::Pointer(Some(ty)) => Some(Ok(*ty)))),
                    ],
                    any_type,
                )?;
                let dest = stack.lookup_out(Out, dest, &T)?;
                let src = stack
                    .lookup(In, src, &Type::Pointer(Some(Box::new(T.clone()))))
                    .or_else(|_| stack.lookup(In, src, &Type::Pointer(None)))?;
                Ok(Self::Deref { T, dest, src })
            }
            ("*", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "*",
                expect: &[2],
                actual: args.len(),
            }),
            ("add", &[dest, lhs, rhs]) => {
                let T = stack.deduce(
                    "T",
                    [(In, lhs, None), (Out, dest, None), (In, rhs, None)],
                    req!(Type::Int | Type::Color | Type::Pointer(Some(_))),
                )?;
                let U = stack.deduce("U", [(In, rhs, None)], |ty| {
                    matches!(
                        (&T, ty),
                        (Type::Int, Type::Int)
                            | (Type::Color, Type::Color | Type::Int)
                            | (Type::Pointer(Some(_)), Type::Int)
                    )
                })?;
                let dest = stack.lookup_out(Out, dest, &T)?;
                let lhs = stack.lookup(In, lhs, &T)?;
                let rhs = stack.lookup(In, rhs, &U)?;
                Ok(match (&T, &U) {
                    (Type::Int, Type::Int) => Self::AddInts { dest, lhs, rhs },
                    (Type::Color, Type::Color) => Self::AddColors { dest, lhs, rhs },
                    (Type::Pointer(Some(_)), Type::Int) => Self::AddPtrInt { T, dest, lhs, rhs },
                    _ => unreachable!(),
                })
            }
            ("add", &[dest, rhs]) => {
                let T = stack.deduce(
                    "T",
                    [(InOut, dest, None)],
                    req!(Type::Int | Type::Color | Type::Pointer(Some(_))),
                )?;
                let U = stack.deduce("U", [(In, rhs, None)], |ty| {
                    matches!(
                        (&T, ty),
                        (Type::Int, Type::Int)
                            | (Type::Color, Type::Color | Type::Int)
                            | (Type::Pointer(Some(_)), Type::Int)
                    )
                })?;
                let dest = stack.lookup_out(InOut, dest, &T)?;
                let rhs = stack.lookup(In, rhs, &U)?;
                Ok(match (&T, &U) {
                    (Type::Int, Type::Int) => Self::AddAssignInts { dest, rhs },
                    (Type::Color, Type::Color) => Self::AddAssignColors { dest, rhs },
                    (Type::Pointer(Some(_)), Type::Int) => Self::AddAssignPtrInt { T, dest, rhs },
                    _ => unreachable!(),
                })
            }
            ("add", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "add",
                expect: &[1, 2],
                actual: args.len(),
            }),
            ("getpx", &[dest, x, y]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Color)?;
                let x = stack.lookup(In, x, &Type::Int)?;
                let y = stack.lookup(In, y, &Type::Int)?;
                Ok(Self::GetPixel { dest, x, y })
            }
            ("getpx", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "getpx",
                expect: &[3],
                actual: args.len(),
            }),
            ("print", what) => {
                let what = what
                    .iter()
                    .map(|&what| {
                        let T = stack.deduce("T", [(In, what, None)], any_type)?;
                        let msg = stack.lookup(In, what, &T)?;
                        Ok((T, msg))
                    })
                    .collect::<Result<_, ParseErrorType>>()?;
                Ok(Self::Print { what })
            }
            ("wait", &[ms]) => {
                let ms = stack.lookup(In, ms, &Type::Int)?;
                Ok(Self::Wait { ms })
            }
            ("wait", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "wait",
                expect: &[1],
                actual: args.len(),
            }),
            ("mouse", &[coord, x, y]) => {
                let coord = stack.lookup(In, coord, &Type::Coordinate)?;
                let x = stack.lookup(In, x, &Type::Int)?;
                let y = stack.lookup(In, y, &Type::Int)?;
                Ok(Self::MoveMouse { coord, x, y })
            }
            ("mouse", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "mouse",
                expect: &[3],
                actual: args.len(),
            }),
            ("kb", &[action, key]) => {
                let action = stack.lookup(In, action, &Type::Action)?;
                let key = stack.lookup(In, key, &Type::Key)?;
                Ok(Self::Key { action, key })
            }
            ("kb", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "kb",
                expect: &[2],
                actual: args.len(),
            }),
            ("mb", &[action, button]) => {
                let action = stack.lookup(In, action, &Type::Action)?;
                let button = stack.lookup(In, button, &Type::Button)?;
                Ok(Self::Button { action, button })
            }
            ("mb", _) => Err(ParseErrorType::ArgCountMismatch {
                instruction: "mb",
                expect: &[2],
                actual: args.len(),
            }),
            _ => Err(ParseErrorType::UnknownInstruction(ins.to_string())),
        }
    }
}

impl std::fmt::Display for Instruction {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Set { T, dest, src } => write!(f, "Set<T = {T}>(dest: {dest}, src: {src})"),
            Self::If { cond } => write!(f, "If(cond: {cond})"),
            Self::Goto { label } => write!(f, "Goto(label: {label})"),
            Self::Eq { T, dest, lhs, rhs } => {
                write!(f, "Eq<T = {T}>(dest: {dest}, lhs: {lhs}, rhs: {rhs})")
            }
            Self::Deref { T, dest, src } => {
                write!(f, "Deref<T = {T}>(dest: {dest}, src: {src})")
            }
            Self::AddInts { dest, lhs, rhs } => {
                write!(f, "AddInts(dest: {dest}, lhs: {lhs}, rhs: {rhs})")
            }
            Self::AddColors { dest, lhs, rhs } => {
                write!(f, "AddColors(dest: {dest}, lhs: {lhs}, rhs: {rhs})")
            }
            Self::AddPtrInt { T, dest, lhs, rhs } => {
                write!(f, "AddPtrInt<T: {T}>(dest: {dest}, lhs: {lhs}, rhs: {rhs})")
            }
            Self::AddAssignInts { dest, rhs } => {
                write!(f, "AddAssignInts(dest: {dest}, rhs: {rhs})")
            }
            Self::AddAssignColors { dest, rhs } => {
                write!(f, "AddAssignColors(dest: {dest}, rhs: {rhs})")
            }
            Self::AddAssignPtrInt { T, dest, rhs } => {
                write!(f, "AddAssignPtrInt<T: {T}>(dest: {dest}, rhs: {rhs})")
            }
            Self::GetPixel { dest, x, y } => write!(f, "GetPixel(dest: {dest}, x: {x}, y: {y})"),
            Self::Print { what } => {
                let Ts = what
                    .iter()
                    .map(|(T, _)| format!("{T}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                let msgs = what
                    .iter()
                    .map(|(_, msg)| format!("{msg}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "Print<Ts = {Ts}>(msgs: {msgs})")
            }
            Self::Wait { ms } => write!(f, "Wait(ms: {ms})"),
            Self::MoveMouse { coord, x, y } => {
                write!(f, "MoveMouse(coord: {coord}, x: {x}, y: {y})")
            }
            Self::Key { action, key } => write!(f, "Key(action: {action}, key: {key})"),
            Self::Button { action, button } => {
                write!(f, "Button(action: {action}, button: {button})")
            }
        }
    }
}
