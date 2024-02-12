---
title:                "Läsa in kommandoradsargument"
aliases:
- /sv/rust/reading-command-line-arguments.md
date:                  2024-01-20T17:56:44.488223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Läsning av kommandoradsargument låter programmet hantera indata den får vid start. Programmerare gör detta för att tillåta flexibilitet och användarspecifika inställningar utan att ändra koden.

## How to:
Rusts standardbibliotek har det du behöver. Vi använder `std::env::args` som ger oss tillgång till kommandoradsargumenten.

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Skriver ut argumenten
    for (index, arg) in args.iter().enumerate() {
        println!("Argument {}: {}", index, arg);
    }
}
```
Kör programmet så här:
```bash
$ cargo run -- ett argument "ett annat argument"
```
Förväntad utskrift:
```
Argument 0: target/debug/my_program
Argument 1: ett
Argument 2: argument
Argument 3: ett annat argument
```

## Deep Dive
Kommandoradsargument har använts sedan tidiga datorprogram då användargränssnitt var textbaserade. Alternativ inkluderar att använda miljövariabler eller config-filer för att skicka in inställningar.

Rust använder `std::env::args` som returnerar en iterator. Detta är användbart för att undvika att ladda alla argument i minnet samtidigt; viktigt för stora argumentlistor. Funktionen `args()` ignorerar felaktiga Unicode-argument.

Ett mer robust alternativ är `std::env::args_os`, som hanterar argument som inte är giltig Unicode. För att hantera argument mer detaljerat finns biblioteket `clap`.

## See Also
- [Rust std::env documentation](https://doc.rust-lang.org/std/env/)
- [The Rust Programming Language book on Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
