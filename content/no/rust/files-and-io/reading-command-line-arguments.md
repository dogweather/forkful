---
date: 2024-01-20 17:56:39.992768-07:00
description: "How to: Kj\xF8r programmet med `cargo run Ola`, f\xE5r du ut."
lastmod: '2024-04-05T21:53:41.568272-06:00'
model: gpt-4-1106-preview
summary: "Kj\xF8r programmet med `cargo run Ola`, f\xE5r du ut."
title: Lese kommandolinjeargumenter
weight: 23
---

## How to:
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 1 {
        println!("Hei, {}!", args[1]);
    } else {
        println!("Hei, ukjent!");
    }
}
```
Kjør programmet med `cargo run Ola`, får du ut:
```
Hei, Ola!
```

## Deep Dive
Lese kommandolinjeargumenter har vært en del av programmeringsrutinen siden de tidligste dagene av Unix. I Rust håndterer standardbiblioteket `std::env` dette. Det tilbyr blant annet `args`, som returnerer en iterator for argumentene.

Et alternativ er `clap`-biblioteket, som gir en mer funksjonsrik grensesnitt for å parse kommandolinjeinput. `std::env::args` er greit for enkle behov, men `clap` hjelper med komplekse applikasjoner.

Når du kaller `args()`, det første argumentet er alltid stien til programmet selv. Etterfølgende argumenter er de som ble oppgitt av brukeren.

## See Also
- [The Rust Programming Language – Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [clap-rs – A full-featured, fast Command Line Argument Parser for Rust](https://github.com/clap-rs/clap)
- [std::env documentation](https://doc.rust-lang.org/std/env/)
