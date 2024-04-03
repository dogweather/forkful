---
date: 2024-01-20 17:51:29.618497-07:00
description: 'How to: In Rust, we use the `format!` macro.'
lastmod: '2024-03-13T22:44:59.883483-06:00'
model: gpt-4-1106-preview
summary: In Rust, we use the `format!` macro.
title: Interpolating a string
weight: 8
---

## How to:
In Rust, we use the `format!` macro:

```Rust
fn main() {
    let name = "Ferris";
    let greeting = format!("Hello, {}!", name);
    println!("{}", greeting); // Prints "Hello, Ferris!"
}
```
The `format!` macro works like `println!`, but it returns the formatted string instead of printing it.

## Deep Dive
Rust chose macros like `format!` for string interpolation over in-language syntax. Why? Macros are powerful and flexible—extending language functionality without complex syntax.

Historically, languages like C used functions like `sprintf`, clunky and error-prone. Rust's `format!` macro is safer, preventing common mistakes.

Alternatives exist, like concatenating with `+` or the `format_args!` macro for avoiding heap allocation. But when it comes to ease and clarity, `format!` is king.

Performance note: `format!` allocates memory. For performance-critical code, consider other methods, like writing directly to a buffer.

## See Also
- Official Rust docs on `format!`: https://doc.rust-lang.org/std/macro.format.html
- `format!` versus `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- Rust by Example on formatting: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
