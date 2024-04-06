---
date: 2024-01-20 17:53:16.537521-07:00
description: "Hvordan: Tilbake p\xE5 70-tallet var printf-deklarasjoner greia for\
  \ feils\xF8king. I Rust bruker vi `println!` for vanlig output og `dbg!` makroen\
  \ for debug.\u2026"
lastmod: '2024-04-05T21:53:41.555275-06:00'
model: gpt-4-1106-preview
summary: "Tilbake p\xE5 70-tallet var printf-deklarasjoner greia for feils\xF8king."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## Hvordan:
```Rust
fn main() {
    let rustacean = "Ferris";
    println!("Hei, {}", rustacean);
    dbg!(rustacean);
}
```
Output:
```
Hei, Ferris
[src/main.rs:4] rustacean = "Ferris"
```

## Dypdykk
Tilbake på 70-tallet var printf-deklarasjoner greia for feilsøking. I Rust bruker vi `println!` for vanlig output og `dbg!` makroen for debug. `dbg!` tar eierskap, returnerer verdien, og skriver til standard error, som gjør det lett å skille fra vanlig output. For å se under overflaten, har Rust-typer `Debug` trait som lar de fleste typer formatere for feilsøking. Implementer `Debug` manuelt for custom typer eller legg til `#[derive(Debug)]` over struct-deklarasjonene dine.

## Se Også
- Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/hello/print/debug.html
- The Rust Programming Language - Chapter 5: https://doc.rust-lang.org/book/ch05-00-structs.html
- Rust Documentation on std::fmt: https://doc.rust-lang.org/std/fmt/
