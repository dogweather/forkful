---
date: 2024-01-20 17:53:24.527406-07:00
description: "Jak to zrobi\u0107: Debugowanie wywodzi si\u0119 z czas\xF3w wcze\u015B\
  niejszych komputeryzacji, gdy \"debugowanie\" dos\u0142ownie oznacza\u0142o usuwanie\
  \ owad\xF3w z maszyn. W Rust,\u2026"
lastmod: '2024-04-05T22:50:49.491900-06:00'
model: gpt-4-1106-preview
summary: "Debugowanie wywodzi si\u0119 z czas\xF3w wcze\u015Bniejszych komputeryzacji,\
  \ gdy \"debugowanie\" dos\u0142ownie oznacza\u0142o usuwanie owad\xF3w z maszyn."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## Jak to zrobić:
```Rust
fn main() {
    let debug_variable = "Hello, Rust!";
    println!("Debug info: {:?}", debug_variable);
}
```
Wynik:
```
Debug info: "Hello, Rust!"
```
Użyj `{:?}` w makrze `println!` dla prostego debugowania.

## Deep Dive
Debugowanie wywodzi się z czasów wcześniejszych komputeryzacji, gdy "debugowanie" dosłownie oznaczało usuwanie owadów z maszyn. W Rust, wykorzystujemy `println!` z różnymi formatterami (`{}`, `{:?}`, `{:#?}` itp.) dla różnych celów. Alternatywy to logowanie i używanie narzędzi jak `gdb` czy `lldb`.

Implementacyjnie, Rust używa derive macro `Debug` dzięki `#[derive(Debug)]`, by automatycznie generować implementację formatowania dla dowolnego typu strukturalnego. Jest to zgodne z ideami zero-cost abstractions i kompilacji warunkowej.

## Zobacz także:
- Dokumentacja Rust: https://doc.rust-lang.org/std/fmt/
- Guide to Rustc's diagnostics: https://rustc-dev-guide.rust-lang.org/diagnostics.html
- The Rust Programming Language – Printing Debug Info: https://doc.rust-lang.org/book/ch5-02-example-structs.html
