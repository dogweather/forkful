---
title:                "Lese kommandolinjeargumenter"
html_title:           "Rust: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har brukt et program fra kommandolinjen, har du sannsynligvis lagt merke til bruk av argumenter for å tilpasse eller spesifisere programmet som kjører. Rust, det moderne programmeringsspråket, tilbyr en enkel og effektiv måte å lese og tolke disse kommandolinjeargumentene på. Det kan være nyttig å lære dette for å forbedre eller lage dine egne kommandolinjeapplikasjoner.

## Hvordan

For å lese kommandolinjeargumenter i Rust, må du ta i bruk et bibliotek kalt std::env. Først må du importere dette biblioteket ved å bruke `use std::env;`, deretter kan du bruke funksjonen `args()` for å få en iterator over argumentene gitt til programmet som et `String`-objekt. Dette gjøres som følger:

```Rust
use std::env;

fn main() {
    // Henter kommandolinjeargumenter og legger dem inn i en vektor
    let args: Vec<String> = env::args().collect();

    // Skriver ut hver argument
    for arg in args {
        println!("{}", arg);
    }
}
```

Kjører du programmet med "hello world!" som argument vil det da skrive ut følgende:

```
$ ./program hello world!
hello world!
```

Det er også mulig å spesifisere hvilket argument du vil lese ved å bruke `nth(i)`-funksjonen på iterator-objektet. Dette vil returnere et `Option`-objekt, som kan inneholde verdien av argumentet eller være tomt hvis argumentet ikke eksisterer. Et eksempel på dette er som følger:

```Rust
use std::env;

fn main() {
    // Henter kommandolinjeargumenter og legger dem inn i en vektor
    let args: Vec<String> = env::args().collect();

    // Leser det andre argumentet og skriver det ut
    match args.get(1) {
        Some(arg) => println!("Det andre argumentet er: {}", arg),
        None => println!("Det andre argumentet finnes ikke"),
    }
}
```

Kjører du programmet med "hello world!" som argument vil det nå skrive ut "Det andre argumentet er: world!".

## Dypdykk

Det er også mulig å lese og tolke andre typer kommandolinjeargumenter, som for eksempel flagg og option-argumenter. Dette gjøres ved å bruke et tredjepartsbibliotek kalt clap, som tilbyr en enkel og elegant måte å håndtere komplekse argumenter. Her er et eksempel på hvordan man kan bruke clap for å håndtere flagg:

```Rust
use clap::{Arg, App};

fn main() {
    // Oppretter en applikasjonsinstans med navnet "program"
    // og legger til en parameter "verbose" som skal være et boolean flagg
    let matches = App::new("program")
                      .arg(Arg::with_name("verbose")
                          .short("v")
                          .long("verbose")
                          .help("Skriver ut mer informasjon"))
                      .get_matches();

    // Sjekker om flagget er satt og skriver det ut
    if matches.is_present("verbose") {
        println!("Flagget var satt!");
    }
}
```

Kjører du nå programmet med flagget "--verbose" vil det skrive ut "Flagget var satt!".

## Se også

- [Rust Dokumentasjon - std::env](https://doc.rust-lang.org/std/env/index.html)
- [Rust Dokumentasjon - clap](https://clap.rs/)