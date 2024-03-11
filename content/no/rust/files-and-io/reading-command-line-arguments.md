---
date: 2024-01-20 17:56:39.992768-07:00
description: "Kommandolinjeargumenter lar deg ta input direkte ved programstart. Vi\
  \ bruker dem for \xE5 tilpasse programkj\xF8ring uten \xE5 endre koden."
lastmod: '2024-03-11T00:14:14.126896-06:00'
model: gpt-4-1106-preview
summary: "Kommandolinjeargumenter lar deg ta input direkte ved programstart. Vi bruker\
  \ dem for \xE5 tilpasse programkj\xF8ring uten \xE5 endre koden."
title: Lese kommandolinjeargumenter
---

{{< edit_this_page >}}

## What & Why?
Kommandolinjeargumenter lar deg ta input direkte ved programstart. Vi bruker dem for å tilpasse programkjøring uten å endre koden.

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
