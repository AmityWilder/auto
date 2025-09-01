use enigo::{Button, Coordinate, Direction, Key};

use crate::{
    input::{KeyboardKey, MouseButton},
    lang::{
        address::{AddressRange, UAddr},
        parse::VarTable,
        run::RuntimeError,
    },
    screen::ColorRGB,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int,
    Str,
    Color,
    Key,
    Button,
    Action,
    Coordinate,
    Union(Box<[Type]>),
    Pointer(Option<Box<Type>>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Color => write!(f, "Color"),
            Type::Key => write!(f, "Key"),
            Type::Button => write!(f, "Button"),
            Type::Action => write!(f, "Action"),
            Type::Coordinate => write!(f, "Coordinate"),
            Type::Union(tys) => {
                write!(
                    f,
                    "({})",
                    tys.iter()
                        .map(|ty| format!("{ty}"))
                        .collect::<Vec<_>>()
                        .join(" | "),
                )
            }
            Type::Pointer(None) => write!(f, "Void*"),
            Type::Pointer(Some(ty)) => write!(f, "{ty}*"),
        }
    }
}

impl Type {
    pub(super) fn discriminant_size(union_tys: &[Type]) -> Result<UAddr, RuntimeError> {
        let num_variants = union_tys.len() as u128;
        [
            (u8::MAX as u128, size_of::<u8>() as UAddr),
            (u16::MAX as u128, size_of::<u16>() as UAddr),
            (u32::MAX as u128, size_of::<u32>() as UAddr),
        ]
        .iter()
        .copied()
        .find(|(max_variants, _)| &num_variants <= max_variants)
        .map(|(_, num_bytes)| num_bytes)
        .ok_or(RuntimeError::UnionSizeOverflow(num_variants))
    }

    pub(super) fn union_variant<'a, 'b>(
        union_tys: &'a [Type],
        value: &'b [u8],
    ) -> Result<(&'a Type, &'b [u8]), RuntimeError> {
        let discriminant_size = Type::discriminant_size(union_tys)?;
        let (discr, value) = value.split_at(discriminant_size as usize);
        let discr = match *discr {
            [b0] => u8::from_ne_bytes([b0]) as usize,
            [b0, b1] => u16::from_ne_bytes([b0, b1]) as usize,
            [b0, b1, b2, b3] => u32::from_ne_bytes([b0, b1, b2, b3]) as usize,
            _ => unreachable!(),
        };
        Ok((&union_tys[discr as usize], value))
    }

    #[inline]
    pub fn size(&self) -> Result<UAddr, RuntimeError> {
        use std::mem::size_of;
        match self {
            Self::Bool => Ok(size_of::<bool>() as UAddr),
            Self::Int => Ok(size_of::<i32>() as UAddr),
            Self::Str => Ok(size_of::<String>() as UAddr),
            Self::Color => Ok(size_of::<ColorRGB>() as UAddr),
            Self::Key => Ok(size_of::<Key>() as UAddr),
            Self::Button => Ok(size_of::<Button>() as UAddr),
            Self::Action => Ok(size_of::<Direction>() as UAddr),
            Self::Coordinate => Ok(size_of::<Coordinate>() as UAddr),
            Self::Union(tys) => Self::discriminant_size(tys)?
                .checked_add(
                    tys.iter()
                        .try_fold(0, |max, ty| ty.size().map(|size| max.max(size)))?,
                )
                .ok_or_else(|| RuntimeError::TypeSizeOverflow(self.clone())),
            Self::Pointer(_) => Ok(size_of::<AddressRange>() as UAddr),
        }
    }

    pub(super) fn try_parse_imm(&self, s: &str, stack: &VarTable) -> Option<Box<[u8]>> {
        match self {
            Self::Bool => match s {
                "True" => Some(true),
                "False" => Some(false),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::Int => s.parse::<i32>().ok().map(i32::to_ne_bytes).map(Box::from),
            Self::Str => {
                let s = s
                    .strip_prefix('"')?
                    .strip_suffix('"')?
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\");
                Some(Box::from(s.as_bytes()))
            }
            Self::Color => s
                .parse::<ColorRGB>()
                .ok()
                .map(|x| [x.r, x.g, x.b])
                .map(Box::from),
            Self::Key => s
                .parse::<KeyboardKey>()
                .ok()
                .map(KeyboardKey::to_ne_bytes)
                .map(Box::from),
            Self::Button => s
                .parse::<MouseButton>()
                .ok()
                .map(MouseButton::to_ne_bytes)
                .map(Box::from),
            Self::Action => match s {
                "Press" => Some(Direction::Press),
                "Release" => Some(Direction::Release),
                "Click" => Some(Direction::Click),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::Coordinate => match s {
                "Abs" => Some(Coordinate::Abs),
                "Rel" => Some(Coordinate::Rel),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::Union(tys) => tys.iter().find_map(|ty| ty.try_parse_imm(s, stack)),
            Self::Pointer(ty) => {
                if let Some(s) = s.strip_prefix('&') {
                    stack
                        .get(s)
                        .and_then(|(var_ty, loc)| (ty.as_deref().is_none_or(|ty| var_ty == ty)).then_some(loc))
                        .map(|loc| Box::from(unsafe {
                            std::mem::transmute::<AddressRange, [u8; size_of::<AddressRange>()]>(*loc)
                        }))
                } else if s == "Null" {
                    Some(Box::from([0; size_of::<AddressRange>()]))
                } else {
                    None
                }
            }
        }
    }

    pub(super) fn try_deduce_imm(s: &str, stack: &VarTable) -> Option<Self> {
        match s {
            "True" | "False" => Some(Self::Bool),
            "Abs" | "Rel" => Some(Self::Coordinate),
            "Null" => Some(Self::Pointer(None)),
            _ => {
                if let Some(s) = s.strip_prefix('&') {
                    stack
                        .get(s)
                        .map(|(ty, _)| Type::Pointer(Some(Box::new(ty.clone()))))
                } else if s.parse::<i32>().is_ok() {
                    Some(Self::Int)
                } else if s.starts_with('#') {
                    Some(Self::Color)
                } else if s.starts_with('"') && s.ends_with('"') {
                    Some(Self::Str)
                } else if s.parse::<KeyboardKey>().is_ok() {
                    Some(Self::Key)
                } else if s.parse::<MouseButton>().is_ok() {
                    Some(Self::Button)
                } else {
                    None // todo
                }
            }
        }
    }
}
