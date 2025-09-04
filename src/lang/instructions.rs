#![allow(non_snake_case)]

use enigo::{Button, Coordinate, Direction, Key};

use crate::{
    lang::{
        address::AddressRange,
        compiler::parse::{ParseErrorType, VarTable},
        memory::Source,
        types::Type,
    },
    screen::ColorRGB,
};

use super::{
    address::{Address, TypedAddress},
    memory::TypedSource,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstructionId {
    Set,
    Deref,
    If,
    Goto,
    /// - [`Instruction::Not`]
    /// - [`Instruction::NotAssign`]
    Not,
    Eq,
    /// - [`Instruction::AddInts`]
    /// - [`Instruction::AddColors`]
    /// - [`Instruction::AddPtrInt`]
    /// - [`Instruction::AddAssignInts`]
    /// - [`Instruction::AddAssignColors`]
    /// - [`Instruction::AddAssignPtrInt`]
    Add,
    GetPixel,
    Print,
    Wait,
    MoveMouse,
    Key,
    Button,
}

impl std::fmt::Display for InstructionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Set => "set",
            Self::Deref => "*",
            Self::If => "if",
            Self::Goto => "goto",
            Self::Not => "not",
            Self::Eq => "eq",
            Self::Add => "add",
            Self::GetPixel => "getpx",
            Self::Print => "print",
            Self::Wait => "wait",
            Self::MoveMouse => "mouse",
            Self::Key => "kb",
            Self::Button => "mb",
        }
        .fmt(f)
    }
}

impl std::str::FromStr for InstructionId {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "set" => Ok(Self::Set),
            "*" => Ok(Self::Deref),
            "if" => Ok(Self::If),
            "goto" => Ok(Self::Goto),
            "not" => Ok(Self::Not),
            "eq" => Ok(Self::Eq),
            "add" => Ok(Self::Add),
            "getpx" => Ok(Self::GetPixel),
            "print" => Ok(Self::Print),
            "wait" => Ok(Self::Wait),
            "mouse" => Ok(Self::MoveMouse),
            "kb" => Ok(Self::Key),
            "mb" => Ok(Self::Button),
            _ => Err(()),
        }
    }
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
        cond: TypedSource<bool>,
    },
    Goto {
        label: TypedSource<Address>,
    },
    Not {
        dest: TypedAddress<bool>,
        value: TypedSource<bool>,
    },
    NotAssign {
        dest: TypedAddress<bool>,
    },
    Eq {
        T: Type,
        dest: TypedAddress<bool>,
        lhs: Source,
        rhs: Source,
    },
    AddInts {
        dest: TypedAddress<i32>,
        lhs: TypedSource<i32>,
        rhs: TypedSource<i32>,
    },
    AddColors {
        dest: TypedAddress<ColorRGB>,
        lhs: TypedSource<ColorRGB>,
        rhs: TypedSource<ColorRGB>,
    },
    AddPtrInt {
        T: Type,
        dest: AddressRange,
        lhs: Source,
        rhs: TypedSource<i32>,
    },
    AddAssignInts {
        dest: TypedAddress<i32>,
        rhs: TypedSource<i32>,
    },
    AddAssignColors {
        dest: TypedAddress<ColorRGB>,
        rhs: TypedSource<ColorRGB>,
    },
    AddAssignPtrInt {
        T: Type,
        dest: AddressRange,
        rhs: Source,
    },
    GetPixel {
        dest: TypedAddress<ColorRGB>,
        x: TypedSource<i32>,
        y: TypedSource<i32>,
    },
    Print {
        what: Box<[(Type, Source)]>,
    },
    Wait {
        ms: TypedSource<i32>,
    },
    MoveMouse {
        coord: TypedSource<Coordinate>,
        x: TypedSource<i32>,
        y: TypedSource<i32>,
    },
    Key {
        action: TypedSource<Direction>,
        key: TypedSource<Key>,
    },
    Button {
        action: TypedSource<Direction>,
        button: TypedSource<Button>,
    },
}

impl Instruction {
    pub const fn id(&self) -> InstructionId {
        match self {
            Self::Set { .. } => InstructionId::Set,
            Self::Deref { .. } => InstructionId::Deref,
            Self::If { .. } => InstructionId::If,
            Self::Goto { .. } => InstructionId::Goto,
            Self::Not { .. } => InstructionId::Not,
            Self::NotAssign { .. } => InstructionId::Not,
            Self::Eq { .. } => InstructionId::Eq,
            Self::AddInts { .. } => InstructionId::Add,
            Self::AddColors { .. } => InstructionId::Add,
            Self::AddPtrInt { .. } => InstructionId::Add,
            Self::AddAssignInts { .. } => InstructionId::Add,
            Self::AddAssignColors { .. } => InstructionId::Add,
            Self::AddAssignPtrInt { .. } => InstructionId::Add,
            Self::GetPixel { .. } => InstructionId::GetPixel,
            Self::Print { .. } => InstructionId::Print,
            Self::Wait { .. } => InstructionId::Wait,
            Self::MoveMouse { .. } => InstructionId::MoveMouse,
            Self::Key { .. } => InstructionId::Key,
            Self::Button { .. } => InstructionId::Button,
        }
    }
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

impl std::fmt::Display for Instruction {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Set { T, dest, src } => write!(f, "Set<T = {T}>(dest: {dest}, src: {src})"),
            Self::If { cond } => write!(f, "If(cond: {cond})"),
            Self::Goto { label } => write!(f, "Goto(label: {label})"),
            Self::Not { dest, value } => {
                write!(f, "Not(dest: {dest}, value: {value})")
            }
            Self::NotAssign { dest } => {
                write!(f, "NotAssign(dest: {dest})")
            }
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
