---
title:    "Rust: Lesing av kommandolinjeargumenter"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av kommandolinje-argumenter er en grunnleggende ferdighet for enhver programmerer, uansett hvilket språk de bruker. I Rust, en moderne programmeringsspråk med fokus på ytelse og pålitelighet, er det flere måter å lese og håndtere kommandolinje-argumenter på. I denne bloggposten vil vi utforske hvordan du enkelt kan lese og behandle kommandolinje-argumenter i Rust.

## Hvordan

For å lese kommandolinje-argumenter i Rust, må du bruke standardbiblioteket `std::env` og deretter bruke funksjonen `args()` for å hente argumentene som en iterator. La oss se på et eksempel:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Antall argumenter: {}", args.len());

    for argument in args.iter() {
        println!("{}", argument);
    }
}
```

I dette eksempelet bruker vi `args()` for å hente alle argumentene og deretter legge dem til i en vektor. Deretter printer vi ut antall argumenter som ble sendt inn, og deretter printer vi ut hvert argument på en ny linje. La oss si at vi kjører programmet med følgende kommandolinje-argumenter: `./program arg1 arg2 arg3`. Da vil utgangen bli:

```
Antall argumenter: 4
./program
arg1
arg2
arg3
```

## Dypdykk

I Rust, kan du også spesifisere hvilke argumenter du forventer å motta ved å bruke `args_os()` i stedet for `args()`. Dette er spesielt nyttig hvis du håndterer argumenter som ikke kan konverteres til UTF-8, som for eksempel filbaner. I tillegg kan du vise en tilpasset hjelpetekst ved å bruke `std::env::args()` i kombinasjon med `println!()` makroen.

Det er også verdt å nevne at det finnes forskjellige biblioteker og rammeverk tilgjengelig i Rust for å gjøre håndteringen av kommandolinje-argumenter enda enklere og mer fleksibel. Noen av disse inkluderer:

- `clap`: Et kraftig og intuitivt bibliotek for å lese og håndtere kommandolinje-argumenter. Det har også støtte for å generere hjelpetekster og underkommandoer.
- `structopt`: En rustrelatert makro for å lage en kommandolinje-grensesnitt(rust related macro for parsing command line interface (CLI)) direkte fra en Rust-struktur.
- `docopt`: En rammeverk som bruker en hjelpetekst for å generere en parser for å håndtere kommandolinje-argumenter.

## Se også

- [Rust documentation: std::env](https://doc.rust-lang.org/std/env/index.html)
- [Clap: A full-featured, fast Command Line Argument Parser for Rust](https://clap.rs/)
- [Structopt: Declarative Parsing for Structs in Rust](https://docs.rs/structopt/0.3.11/structopt/)
- [Docopt: A Rust implementation of Docopt language](https://docs.rs/docopt/1.1.0/docopt/)

Vi håper denne bloggposten har gitt deg en grundig forståelse for hvordan du kan lese og håndtere kommandolinje-argumenter i Rust. Husk å sjekke ut de ulike bibliotekene og rammeverkene nevnt ovenfor for å finne den som passer best for ditt prosjekt. Lykke til med din Rust-programmering!