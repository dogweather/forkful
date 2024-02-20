---
date: 2024-01-20 17:56:44.488223-07:00
description: "L\xE4sning av kommandoradsargument l\xE5ter programmet hantera indata\
  \ den f\xE5r vid start. Programmerare g\xF6r detta f\xF6r att till\xE5ta flexibilitet\
  \ och\u2026"
lastmod: 2024-02-19 22:04:56.918990
model: gpt-4-1106-preview
summary: "L\xE4sning av kommandoradsargument l\xE5ter programmet hantera indata den\
  \ f\xE5r vid start. Programmerare g\xF6r detta f\xF6r att till\xE5ta flexibilitet\
  \ och\u2026"
title: "L\xE4sa in kommandoradsargument"
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
