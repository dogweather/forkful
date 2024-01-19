---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinjeargumenter er prosessen der programmet ditt mottar data direkte når det er kjørt. Dette tillater brukeren å påvirke programmets oppførsel uten å måtte endre selve koden.

## Hvordan:

Rust har et modul `std::env` som kan brukes for å lese kommandolinje-argumenter. Her er et eksempel:

```rust
// Importerer 'std::env'
use std::env;

fn main() {
    // Bruker 'args()' funksjonen til å hente argumentene
    let args: Vec<String> = env::args().collect();

    // Skriver ut hvert argument
    for arg in args {
        println!("{}", arg);
    }
}
```

Når du kjører programmet og gir argumenter til kommandolinjen, vil output være som følger:

```console
$ rustc main.rs
$ ./main HelloWorld! Jeg er Rust programmerer.
./main
HelloWorld!
Jeg
er
Rust
programmerer.
```

## Deep Dive:

Å lese kommandolinje-argumenter er ikke begrenset til Rust, det har blitt brukt i programmering lenge og er tilgjengelig i nesten alle programmeringsspråk. Det gir en interaktiv brukeropplevelse og gjør programmene mer fleksible.

Det finnes også biblioteker som `getopts` og `clap` i Rust-økosystemet som gir mer avanserte funksjonaliteter for å håndtere kommandolinje-argumenter, som å gi standardverdier, tvinge bestemte typer, avkortinger og mer.

Å lese kommandolinje-argumenter i Rust er ganske direkte. `std::env::args()` returnerer en iterator over argumentene. Det første argumentet er tradisjonelt banen til programmet som kjører. De påfølgende argumentene er gitt verdiene som er bestemt av kommandolinjen.

## Se Også:

- Rust Std Lib: [`std::env::args`](https://doc.rust-lang.org/std/env/fn.args.html)
- Rust Doc: [Kommandolinje-argumenter i Rust](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- Rust Bibliotek: [getopts](https://docs.rs/getopts/0.2.21/getopts/)
- Rust Bibliotek: [clap](https://docs.rs/clap/2.33.3/clap/)