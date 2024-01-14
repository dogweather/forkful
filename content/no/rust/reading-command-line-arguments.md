---
title:                "Rust: Å lese kommandolinjeargumenter"
simple_title:         "Å lese kommandolinjeargumenter"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se på hvordan man kan lese kommandolinje-argumenter ved hjelp av Rust, et moderne programmeringsspråk som er kjent for sin pålitelighet og effektive ytelse. Å kunne lese kommandolinje-argumenter er en viktig del av å lage robuste og fleksible programmer, og det er derfor verdt å lære hvordan man gjør det i Rust.

## Slik gjør du det

For å kunne lese kommandolinje-argumenter i Rust, bruker vi standardbiblioteket "std::env". Dette gir oss tilgang til funksjonen "args()", som returnerer en vector med alle argumentene som ble gitt til programmet.

La oss se på et enkelt eksempel. I dette eksempelet ønsker vi å lage et program som tar inn et navn og printer en personlig hilsen til terminalen:

```rust
use std::env;

fn main() {
    // Leser kommandolinje-argumenter
    let args: Vec<String> = env::args().collect();

    // Sjekker om det ble gitt et navn som argument
    if args.len() > 1 {
        // Første argument er navnet som ble gitt
        let name = &args[1];
        println!("Hei, {}!", name);
    } else {
        println!("Vennligst skriv inn et navn som argument.");
    }
}
```

Når vi kjører dette programmet med kommandoen "rustc hello.rs && ./hello John", vil terminalen skrive ut "Hei, John!". Hvis vi ikke gir et navn som argument, vil den skrive ut "Vennligst skriv inn et navn som argument.".

Vi kan også benytte oss av "args()" funksjonen for å lese flere argumenter, og behandle dem på ulike måter i programmet vårt.

## Dypdykk

Når vi bruker "args()" funksjonen, får vi en vector med argumenter som er gitt som strenger. Det kan være nyttig å konvertere disse argumentene til andre datatyper hvis vi ønsker å gjøre mer sofistikerte operasjoner med dem. For eksempel kan vi bruke "parse()" funksjonen for å konvertere en streng til et tall. Her er et eksempel:

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        // Første argument er et tall
        let num1: i32 = args[1].parse().unwrap();
        // Andre argument er også et tall
        let num2: i32 = args[2].parse().unwrap();

        let sum = num1 + num2;
        println!("Summen av {} og {} er {}.", num1, num2, sum);
    } else {
        println!("Vennligst skriv inn to tall som argumenter.");
    }
}
```

Nå kan vi kjøre programmet med to tall som argumenter, for eksempel "rustc sum.rs && ./sum 5 10", og terminalen vil skrive ut "Summen av 5 og 10 er 15.".

## Se også

- [Rust Documentation - std::env](https://doc.rust-lang.org/std/env/)
- [Rust by Example - Command Line Arguments](https://doc.rust-lang.org/stable/rust-by-example/std_misc/arg.html)