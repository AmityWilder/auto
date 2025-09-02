// #![deny(clippy::unwrap_used, reason = "prefer errors")]
#![cfg(not(target_pointer_width = "16"))]

mod address;
pub mod compiler;
pub mod instructions;
mod memory;
pub mod run;
mod types;
