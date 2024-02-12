---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/rust/printing-debug-output/
date:                  2024-01-20T17:53:16.537521-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskriving av feilsøkingsdata i Rust lar deg se hva som foregår under panseret i koden din. Programmere gjør dette for å spore flyten og oppdage bugs.

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
