---
title:                "Rust: Lesing av kommandolinje-argumenter"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmer krever interaksjon med brukeren, og et vanlig eksempel på dette er å lese inn kommandolinjeargumenter. Dette lar programmet ta imot informasjon fra brukeren og utføre handlinger basert på disse argumentene. Lesing av kommandolinjeargumenter er derfor en viktig ferdighet for enhver programmerer, og i denne artikkelen vil vi se på hvordan man kan gjøre dette i Rust.

## Hvordan

For å lese kommandolinjeargumenter i Rust, kan vi bruke et innebygd bibliotek som heter `args`. Vi trenger først å importere dette biblioteket i vårt program ved å legge til `use std::env;` i toppen av koden vår. Deretter kan vi bruke funksjonen `args()` fra dette biblioteket for å få en vektor med alle de gitte argumentene.

For å få tilgang til de enkelte argumentene, kan vi bruke indeksering på vektoren vår. For eksempel, hvis vi ønsker å lese inn det første argumentet, kan vi skrive `args[1]` (indeksering starter på 1, siden det første argumentet er selve programnavnet).

La oss se på et eksempel på hvordan dette kan se ut:

```rust
use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Programnavn: {}", args[0]);
    println!("Første argument: {}", args[1]);
}
```

Her oppretter vi først en vektor ved hjelp av `args()`-funksjonen og lagrer den i en variabel kalt `args`. Deretter bruker vi `println!()`-makroen for å skrive ut programnavnet og det første argumentet. For å kjøre dette programmet, må vi først kompilere det og deretter kan vi gi argumentene vi ønsker å lese inn i kommandolinjen. For eksempel kan vi skrive `./programnavn argument_1 argument_2` og få output som følger:

```bash
Programnavn: programnavn
Første argument: argument_1
```

## Deep Dive

Nå som vi har sett hvordan man enkelt kan lese inn kommandolinjeargumenter i Rust, la oss se på noen flere detaljer om dette. Det er viktig å merke seg at når man leser inn argumenter, vil alle disse være `String`-typer, uavhengig av hva slags datatype det faktiske argumentet er. Hvis vi ønsker å konvertere et argument til en annen datatype, må vi gjøre dette manuelt ved å bruke funksjoner som `parse()` og `unwrap()`.

Vi kan også legge til funksjonalitet for å sjekke om riktig antall argumenter er gitt, eller for å gi en brukerfeilmelding hvis et forventet argument ikke er gitt. Dette kan være nyttig for å gjøre programmet vårt mer robust og brukervennlig.

## Se også

For mer informasjon om lesing av kommandolinjeargumenter i Rust, kan du besøke følgende ressurser:

- [Rust Docs: Command Line Arguments](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rust by Example: Command Line Arguments](https://doc.rust-lang.org/rust-by-example/std_misc/arg.html)
- [The Rust Programming Language: Handling Command Line Arguments](https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html#handling-command-line-arguments)

Takk for at du leste denne artikkelen om å lese kommandolinjeargumenter i Rust. Vi håper den var nyttig og gir deg en god forståelse av hvordan man kan gjøre dette i ditt eget prosjekt. Lykke til med programmeringen!