---
title:                "Drukowanie komunikatów debugowania"
aliases:
- /pl/rust/printing-debug-output.md
date:                  2024-01-20T17:53:24.527406-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wypisywanie debugowania to wyświetlanie informacji o działaniu programu – pomaga to programistom łapać błędy. Wykorzystujemy to, aby zobaczyć, co się dzieje w naszym kodzie, zanim coś pójdzie nie tak.

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
