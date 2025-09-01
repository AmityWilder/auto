#![allow(non_snake_case)]

use crate::lang::{
    address::AddressRange,
    memory::Source,
    parse::{ParseErrorType, VarTable},
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
    Add {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    Sub {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    Mul {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    Div {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    Rem {
        dest: AddressRange,
        lhs: Source,
        rhs: Source,
    },
    AddAssign {
        dest: AddressRange,
        rhs: Source,
    },
    SubAssign {
        dest: AddressRange,
        rhs: Source,
    },
    MulAssign {
        dest: AddressRange,
        rhs: Source,
    },
    DivAssign {
        dest: AddressRange,
        rhs: Source,
    },
    RemAssign {
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

impl Instruction {
    pub(super) fn from_str<'a>(
        stack: &mut VarTable,
        ins: &'a str,
        args: &[&'a str],
    ) -> Result<Instruction, ParseErrorType> {
        use ArgDirection::*;
        match (ins, args) {
            ("set", &[dest, src]) => {
                let T = stack
                    .typecheck(In, src)
                    .or_else(|| stack.typecheck(Out, dest))
                    .ok_or(ParseErrorType::TypeDeductionFailed("T"))
                    .flatten()?;
                let dest = stack.lookup_out(Out, dest, &T)?;
                let src = stack.lookup(In, src, &T)?;
                Ok(Self::Set { T, dest, src })
            }
            ("*", &[dest, src]) => {
                let T = match stack.typecheck(In, src) {
                    Some(Ok(Type::Pointer(Some(ty)))) => Some(Ok(*ty)),
                    Some(Ok(Type::Pointer(None) | _)) => None,
                    x @ (Some(Err(_)) | None) => x,
                }
                .or_else(|| stack.typecheck(Out, dest))
                .ok_or(ParseErrorType::TypeDeductionFailed("T"))
                .flatten()?;
                let dest = stack.lookup_out(Out, dest, &T)?;
                let src = stack
                    .lookup(In, src, &Type::Pointer(Some(Box::new(T.clone()))))
                    .or_else(|_| stack.lookup(In, src, &Type::Pointer(None)))?;
                Ok(Self::Deref { T, dest, src })
            }
            ("add", &[dest, lhs, rhs]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Int)?;
                let lhs = stack.lookup(In, lhs, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::Add { dest, lhs, rhs })
            }
            ("sub", &[dest, lhs, rhs]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Int)?;
                let lhs = stack.lookup(In, lhs, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::Sub { dest, lhs, rhs })
            }
            ("mul", &[dest, lhs, rhs]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Int)?;
                let lhs = stack.lookup(In, lhs, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::Mul { dest, lhs, rhs })
            }
            ("div", &[dest, lhs, rhs]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Int)?;
                let lhs = stack.lookup(In, lhs, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::Div { dest, lhs, rhs })
            }
            ("rem", &[dest, lhs, rhs]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Int)?;
                let lhs = stack.lookup(In, lhs, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::Rem { dest, lhs, rhs })
            }
            ("add", &[dest, rhs]) => {
                let dest = stack.lookup_out(InOut, dest, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::AddAssign { dest, rhs })
            }
            ("sub", &[dest, rhs]) => {
                let dest = stack.lookup_out(InOut, dest, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::SubAssign { dest, rhs })
            }
            ("mul", &[dest, rhs]) => {
                let dest = stack.lookup_out(InOut, dest, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::MulAssign { dest, rhs })
            }
            ("div", &[dest, rhs]) => {
                let dest = stack.lookup_out(InOut, dest, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::DivAssign { dest, rhs })
            }
            ("rem", &[dest, rhs]) => {
                let dest = stack.lookup_out(InOut, dest, &Type::Int)?;
                let rhs = stack.lookup(In, rhs, &Type::Int)?;
                Ok(Self::RemAssign { dest, rhs })
            }
            ("getpx", &[dest, x, y]) => {
                let dest = stack.lookup_out(Out, dest, &Type::Color)?;
                let x = stack.lookup(In, x, &Type::Int)?;
                let y = stack.lookup(In, y, &Type::Int)?;
                Ok(Self::GetPixel { dest, x, y })
            }
            ("print", what) => {
                let what = what
                    .iter()
                    .map(|what| {
                        let T = stack
                            .typecheck(In, what)
                            .ok_or(ParseErrorType::TypeDeductionFailed("T"))
                            .flatten()?;
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
            ("mouse", &[coord, x, y]) => {
                let coord = stack.lookup(In, coord, &Type::Coordinate)?;
                let x = stack.lookup(In, x, &Type::Int)?;
                let y = stack.lookup(In, y, &Type::Int)?;
                Ok(Self::MoveMouse { coord, x, y })
            }
            ("kb", &[action, key]) => {
                let action = stack.lookup(In, action, &Type::Action)?;
                let key = stack.lookup(In, key, &Type::Key)?;
                Ok(Self::Key { action, key })
            }
            ("mb", &[action, button]) => {
                let action = stack.lookup(In, action, &Type::Action)?;
                let button = stack.lookup(In, button, &Type::Button)?;
                Ok(Self::Button { action, button })
            }
            _ => Err(ParseErrorType::UnknownInstruction(ins.to_string())),
        }
    }
}

impl std::fmt::Display for Instruction {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Set { T, dest, src } => write!(f, "Set<T = {T}>(dest: {dest}, src: {src})"),
            Self::Deref { T, dest, src } => {
                write!(f, "Deref<T = {T}>(dest: {dest}, src: {src})")
            }
            Self::Add { dest, lhs, rhs } => write!(f, "Add(dest: {dest}, lhs: {lhs}, rhs: {rhs})"),
            Self::Sub { dest, lhs, rhs } => write!(f, "Sub(dest: {dest}, lhs: {lhs}, rhs: {rhs})"),
            Self::Mul { dest, lhs, rhs } => write!(f, "Mul(dest: {dest}, lhs: {lhs}, rhs: {rhs})"),
            Self::Div { dest, lhs, rhs } => write!(f, "Div(dest: {dest}, lhs: {lhs}, rhs: {rhs})"),
            Self::Rem { dest, lhs, rhs } => write!(f, "Rem(dest: {dest}, lhs: {lhs}, rhs: {rhs})"),
            Self::AddAssign { dest, rhs } => write!(f, "AddAssign(dest: {dest}, rhs: {rhs})"),
            Self::SubAssign { dest, rhs } => write!(f, "SubAssign(dest: {dest}, rhs: {rhs})"),
            Self::MulAssign { dest, rhs } => write!(f, "MulAssign(dest: {dest}, rhs: {rhs})"),
            Self::DivAssign { dest, rhs } => write!(f, "DivAssign(dest: {dest}, rhs: {rhs})"),
            Self::RemAssign { dest, rhs } => write!(f, "RemAssign(dest: {dest}, rhs: {rhs})"),
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
