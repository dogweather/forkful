---
title:                "Å skrive en tekstfil"
html_title:           "Rust: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Å skrive en tekstfil er å lage en fil som inneholder tekst, som kan leses av en datamaskin. Programmere gjør dette for å lagre informasjon som kan brukes senere, enten for å lagre data eller for å lese og manipulere informasjonen.

## Hvordan:
I Rust, kan du enkelt skrive en tekstfil ved å bruke standard biblioteket "std::fs". Først, importer biblioteket ved å legge til linjen "use std::fs;" øverst i filen. Deretter kan du bruke funksjonen "write" for å skrive tekst til en fil, som vist i følgende eksempel:

```Rust
use std::fs;

fn main() {
    let mut file = fs::File::create("tekstfil.txt") // lager en ny fil
        .expect("Kunne ikke lage fil"); // håndterer feil
    file.write(b"Tekst som skal skrives til filen") // skriver tekst ved å konvertere det til bytes
        .expect("Kunne ikke skrive til fil"); // håndterer feil
}
```

Dette vil lage en ny fil med navnet "tekstfil.txt" og skrive teksten "Tekst som skal skrives til filen" til den. Du kan også legge til forskjellige tekstformateringsfunksjoner hvis du ønsker det.

## Dypdykk:
Skriving av tekstfiler har vært en viktig del av programmering siden starten av databehandling. Det finnes også alternative metoder for å lagre informasjon, som for eksempel å lagre data i en database. I Rust, er skriving av tekstfiler implementert ved hjelp av standard biblioteket "std::fs", som gjør det enkelt og effektivt.

## Se også:
For mer informasjon om å skrive tekstfiler i Rust, kan du sjekke ut Rusts offisielle dokumentasjon på det standard biblioteket "std::fs":
https://doc.rust-lang.org/std/fs/