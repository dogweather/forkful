---
title:                "Rust: Skriving av en tekstfil"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan virke som en enkel oppgave, men det er faktisk en viktig del av programmering. Tekstfiler er en måte å lagre data på som kan leses og modifiseres av både mennesker og datamaskiner. Dette gjør det til et nyttig verktøy for å lagre og behandle informasjon i ulike programmer. I denne bloggposten skal vi se på hvordan du kan skrive en tekstfil i Rust, et språk som er kjent for sin effektivitet og sikkerhet.

## Slik gjør du det

Det første du trenger å gjøre er å opprette en fil ved hjelp av `File::create()`-funksjonen. Denne tar inn en filbane som argument og returnerer en `Result` som enten indikerer suksess eller en feilmelding. Deretter kan du bruke `write_all()`-funksjonen for å skrive data til filen du har opprettet. Her er et enkelt eksempel på hvordan du kan skrive en tekstfil med Rust:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("min_fil.txt")?;
    file.write_all(b"Hei, dette er en tekstfil skrevet med Rust!")?;
    Ok(())
}
```

I dette eksemplet opprettes en fil kalt "min_fil.txt" og teksten "Hei, dette er en tekstfil skrevet med Rust!" blir skrevet til filen. Merk at vi bruker `b`-prefikset før teksten for å konvertere den til en byte-streng som kan skrives til filen. Denne koden vil returnere en `Result` som enten er `Ok` hvis skrivingen var vellykket, eller en `Err` hvis det oppstod en feil.

## Dykk dypere

Når du skal skrive en tekstfil, kan det være nyttig å vite hvordan du kan formatere tekst og legge til linjeskift. Rust har flere innebygde makroer som kan hjelpe deg med dette. Her er et eksempel på hvordan du kan skrive en tekstfil med flere linjer og forskjellig formatering:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("min_fil.txt")?;
    write!(file, "Dette er linje nummer 1.\n")?;
    writeln!(file, "Dette er linje nummer 2.")?;
    writeln!(file, "Dette er linje nummer 3 med et tall: {}.", 42)?;
    Ok(())
}
```

I dette eksemplet bruker vi `write!`- og `writeln!`-makroene for å skrive til filen. Disse fungerer på samme måte som `write_all()`, men de lar deg også formatere teksten på en enkel måte. Merk at vi også bruker `\n` for å legge til et linjeskift.

## Se også

- [Rust dokumentasjon om filbehandling](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Lær Rust på 15 minutter](https://www.rust-lang.org/learn/get-started)
- [Rust fellesskapet på Reddit](https://www.reddit.com/r/rust/)