use std::collections::HashMap;

use crate::lang::{
    address::{Address, AddressRange, UAddr},
    run::{Source, Type},
};

#[derive(Debug, Clone)]
pub struct Program(Vec<Instruction>);

impl Program {
    pub fn line(&self, n: usize) -> Option<&Instruction> {
        self.0.get(n)
    }
}

#[derive(Debug)]
pub enum ParseErrorType {
    UnknownInstruction(String),
    UnknownVariable(String),
    InvalidLiteral(Type, String),
    UnknownImmType(String),
    TypeDeductionFailed(usize),
    TooManyArgs {
        instruction: &'static str,
        expect: usize,
        actual: usize,
    },
    MissingArgs {
        instruction: &'static str,
        missing: &'static str,
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
                write!(f, "could not deduce the type of generic argument {id}")
            }
            TooManyArgs {
                instruction: ins,
                expect,
                actual,
            } => write!(
                f,
                "`{ins}` takes {expect} arguments but {actual} were supplied"
            ),
            MissingArgs {
                instruction: ins,
                missing,
            } => write!(
                f,
                "not enough arguments for `{ins}`. missing argument for `{missing}`"
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ArgDirection {
    /// Can accept literals.
    /// Cannot create a new variable.
    /// Can be used to deduce generic types.
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

struct VarTable {
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
                        Type::try_deduce_imm(arg)
                            .ok_or_else(|| ParseErrorType::UnknownImmType(arg.to_string())),
                    )
                }
            }
            // Type can only be deduced if initialized
            InOut => self.vars.get(arg).map(|(ty, _)| Ok(ty.clone())),
            // Type cannot be deduced
            Out => None,
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
                    self.lookup(InOut, arg, ty)
                } else {
                    ty.try_parse_imm(arg)
                        .map(Source::Immediate)
                        .ok_or_else(|| ParseErrorType::InvalidLiteral(ty.clone(), arg.to_string()))
                }
            }

            InOut => {
                let (var_ty, addr) = self
                    .vars
                    .get(arg)
                    .ok_or_else(|| ParseErrorType::UnknownVariable(arg.to_string()))?;

                if var_ty == ty {
                    Ok(Source::Address(*addr))
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
                self.lookup(InOut, arg, ty)
            }
        }
    }
}

#[derive(Debug)]
enum TypeArg {
    Static(Type),
    /// ID of the generic within the function
    Generic(usize),
}

fn get_args(
    stack: &mut VarTable,
    instruction: &'static str,
    generics: &mut [Option<Type>],
    expect_args: &[(ArgDirection, &'static str, TypeArg)],
    args: &[&str],
) -> Result<Vec<Source>, ParseErrorType> {
    match args.len().cmp(&expect_args.len()) {
        std::cmp::Ordering::Greater => Err(ParseErrorType::TooManyArgs {
            instruction,
            expect: expect_args.len(),
            actual: args.len(),
        }),
        std::cmp::Ordering::Less => Err(ParseErrorType::MissingArgs {
            instruction,
            missing: expect_args[args.len()].1,
        }),
        std::cmp::Ordering::Equal => {
            for ((dir, tyid), arg) in
                expect_args
                    .iter()
                    .zip(args.iter().copied())
                    .filter_map(|((d, _, t), a)| match t {
                        &TypeArg::Generic(id) => Some(((*d, id), a)),
                        _ => None,
                    })
            {
                if let Some(ty) = stack.typecheck(dir, arg) {
                    generics[tyid] = Some(ty?);
                }
            }

            expect_args
                .iter()
                .zip(args.iter().copied())
                .map(|((direction, _, ty), arg)| {
                    let ty = match ty {
                        TypeArg::Static(ty) => ty,
                        &TypeArg::Generic(id) => generics[id]
                            .as_ref()
                            .ok_or(ParseErrorType::TypeDeductionFailed(id))?,
                    };
                    stack.lookup(*direction, arg, ty)
                })
                .collect::<Result<Vec<_>, _>>()
        }
    }
}

macro_rules! instructions {
    ($($name:literal => $Variant:ident$(<$($Generic:ident),+>)?($( $(#[$m:meta])* [$dir:ident] $arg:ident: $typekind:ident($type:expr) ),*);)*) => {
        #[derive(Debug, Clone)]
        #[allow(non_snake_case)]
        pub enum Instruction {
            $($Variant {
                $($(
                    $Generic: Type,
                )+)?
                $(
                    #[doc = concat!("[`", stringify!($dir), "`](ArgDirection::", stringify!($dir), ") [`", stringify!($type), "`]")]
                    $(#[$m])*
                    $arg: instructions!(@@ [$dir])
                ),*
            }),*
        }

        impl Instruction {
            fn from_str<'a>(
                stack: &mut VarTable,
                ins: &'a str,
                args: &[&'a str],
            ) -> Result<Instruction, ParseErrorType> {
                use TypeArg::*;
                match ins {
                    $($name => {
                        const GENERICS: [&str; instructions!(# $([$(stringify!($Generic)),+].len())?)] = [$($(stringify!($Generic)),+)?];
                        let mut generics = [const { None }; GENERICS.len()];
                        $(
                        #[allow(non_snake_case)]
                        let [$($Generic),+] = std::array::from_fn(|i| i);
                        )?
                        let Ok([$(instructions!(@ [$dir] $arg)),*]) = <[Source; _]>::try_from(get_args(
                            stack,
                            $name,
                            &mut generics,
                            &[$((ArgDirection::$dir, stringify!($arg), $typekind($type))),*],
                            &args,
                        )?) else {
                            panic!(
                                "get_args should always produce the requested number & kind of arguments"
                            )
                        };
                        $(
                        #[allow(non_snake_case)]
                        let [$($Generic),+] = generics.map(|x| x.expect("should have returned with error during type deduction"));
                        $(println!("deduced {} as {:?}", stringify!($Generic), $Generic);)+
                        )?
                        Ok(Instruction::$Variant { $($($Generic,)+)? $($arg),* })
                    })*
                    _ => Err(ParseErrorType::UnknownInstruction(ins.to_string())),
                }
            }
        }

        impl std::fmt::Display for Instruction {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$Variant { $($($Generic,)+)? $($arg),* } => {
                        f.write_str(stringify!($Variant))?;
                        $(write!(f, "<{}>", [$(format!("{}: {:?}", stringify!($Generic), $Generic)),+].join(", "))?;)?
                        write!(f, "({})", [$(format!("[{}] {} {}: {}", stringify!($dir), stringify!($type), stringify!($arg), $arg)),*].join(", "))?;
                        Ok(())
                    }),*
                }
            }
        }
    };

    (@ [In] $arg:ident) => { $arg };
    (@ [$dir:ident] $arg:ident) => { Source::Address($arg) };

    (@@ [In]) => { Source };
    (@@ [$dir:ident]) => { AddressRange };

    (# $($tokens:tt)+) => { $($tokens)+ };
    (# ) => { 0 };
}

instructions! {
    "set" => Set<T>(
        /// The variable to store to
        [Out] dest: Generic(T),
        /// The value to set with
        [In] src: Generic(T)
    );
    "+" => Add(
        [InOut] dest: Static(Type::Int),
        [In] lhs: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "-" => Sub(
        [InOut] dest: Static(Type::Int),
        [In] lhs: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "*" => Mul(
        [InOut] dest: Static(Type::Int),
        [In] lhs: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "/" => Div(
        [InOut] dest: Static(Type::Int),
        [In] lhs: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "%" => Rem(
        [InOut] dest: Static(Type::Int),
        [In] lhs: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "+=" => AddAssign(
        [InOut] dest: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "-=" => SubAssign(
        [InOut] dest: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "*=" => MulAssign(
        [InOut] dest: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "/=" => DivAssign(
        [InOut] dest: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "%=" => RemAssign(
        [InOut] dest: Static(Type::Int),
        [In] rhs: Static(Type::Int)
    );
    "getpx" => GetPixel(
        [Out] dest: Static(Type::Color),
        [In] x: Static(Type::Int),
        [In] y: Static(Type::Int)
    );
    "print" => Print<T>(
        [In] what: Generic(T)
    );
    "mouse" => MoveMouse(
        [In] coord: Static(Type::Coordinate),
        [In] x: Static(Type::Int),
        [In] y: Static(Type::Int)
    );
    "kb" => Key(
        [In] action: Static(Type::Action),
        [In] key: Static(Type::Key)
    );
    "mb" => Button(
        [In] action: Static(Type::Action),
        [In] button: Static(Type::Button)
    );
}

impl std::str::FromStr for Program {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut stack = VarTable::new(4096);
        let mut args = Vec::new();
        s.lines()
            .map(|line| line[..line.find('#').unwrap_or(line.len())].trim())
            .enumerate()
            .filter(|(_, line)| !line.is_empty())
            .map(|(n, line)| -> Result<Instruction, ParseError> {
                let mut it = line.split_whitespace();
                let ins = it.next().expect("should be guaranteed by filter");
                args.clear();
                args.extend(it);
                println!("line {}: `{line}`", n + 1);
                Instruction::from_str(&mut stack, ins, &args)
                    .inspect(|instruction| println!("parsed: {instruction}"))
                    .map_err(|ty| ParseError {
                        ty,
                        line: n,
                        code: line.to_string(),
                    })
            })
            .collect::<Result<Vec<Instruction>, ParseError>>()
            .map(Program)
    }
}
