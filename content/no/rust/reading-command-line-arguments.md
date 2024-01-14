---
title:    "Rust: Lesing av kommandolinje-argumenter"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Hvorfor lese kommandolinje-argumenter i Rust

Rust er et programmeringsspråk som har blitt stadig mer populært de siste årene, spesielt innenfor systemprogrammering. En av styrkene til Rust er dets evne til å håndtere minnesikkerhet på en effektiv måte. Når det kommer til å lese kommandolinje-argumenter, er Rust et robust språk som kan hjelpe utviklere å lage pålitelige og sikre programmer. I denne bloggposten vil vi se nærmere på hvorfor det er viktig å lese kommandolinje-argumenter og hvordan man gjør det i Rust.

## Hvordan lese kommandolinje-argumenter i Rust

Før vi ser på kodeeksempler, la oss først definere hva kommandolinje-argumenter er og hvorfor de er nyttige i programmering. Kommandolinje-argumenter er en måte å gi input til et program når det kjøres fra en kommandolinje. Dette kan være nyttig for å endre programmet sin kjøreatferd uten å måtte endre koden. For eksempel kan du gi et filnavn som argument for å lese og behandle filen i programmet.

I Rust kan vi lese kommandolinje-argumenter ved hjelp av standardbiblioteket `std::env`. La oss se på et enkelt eksempel på hvordan man kan lese det første argumentet gitt i kommandolinjen:

```Rust
use std::env;
    
let args: Vec<String> = env::args().collect();
    
if let Some(arg) = args.get(1) {
    println!("Det første argumentet er: {}", arg);
} else {
    println!("Ingen kommandolinje-argumenter ble gitt.");
}
```

I dette eksempelet bruker vi `std::env::args()` som returnerer en `Vec<String>` som inneholder alle kommandolinje-argumentene. Vi bruker deretter `.collect()` for å lagre dem i en vektor. Ved å bruke `.get(1)` på vektoren kan vi få tak i det første argumentet (indeks 0 er programmet sitt navn). Vi bruker `if let` for å sjekke om argumentet faktisk finnes før vi skriver det ut. Hvis det ikke finnes noen argumenter, skriver vi ut en passende melding.

## Dypdykk i lesing av kommandolinje-argumenter

I tillegg til å lese enkeltstående argumenter, kan vi også benytte `std::env` for å håndtere ulike typer argumenter. For eksempel kan vi bruke `std::env::args_os()` for å håndtere argumenter som ikke er i UTF-8 format. Vi kan også bruke `std::env::current_dir()` for å få tak i nåværende arbeidsmappe, som kan være nyttig å gi som et argument.

Det finnes også forskjellige tredjeparts biblioteker som kan gjøre det enda enklere å håndtere kommandolinje-argumenter i Rust, for eksempel `clap` og `structopt`.

## Se også

- [Rust dokumentasjon om std::env](https://doc.rust-lang.org/std/env/index.html)
- [Rust dokumentasjon om clap biblioteket for å håndtere kommandolinje-argumenter](https://docs.rs/clap/)
- [Rust dokumentasjon om structopt biblioteket for å lage kommandolinje-grensesnitt](https://docs.rs/structopt/)