// #![deny(clippy::unwrap_used, reason = "prefer errors")]
#![cfg(not(target_pointer_width = "16"))]

mod address;
pub mod parse;
pub mod run;
