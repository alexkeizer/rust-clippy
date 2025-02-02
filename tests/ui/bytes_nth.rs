// run-rustfix

#![allow(clippy::unnecessary_operation)]
#![warn(clippy::bytes_nth)]

fn main() {
    let s = String::from("String");
    let _ = s.bytes().nth(3);
    let _ = &s.bytes().nth(3);
    let _ = s[..].bytes().nth(3);
    let _x: u8 = s.bytes().nth(3).unwrap();
}
