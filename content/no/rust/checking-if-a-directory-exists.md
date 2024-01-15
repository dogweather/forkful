---
title:                "Sjekk om en mappe eksisterer"
html_title:           "Rust: Sjekk om en mappe eksisterer"
simple_title:         "Sjekk om en mappe eksisterer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er et viktig skritt i mange programmeringsprosjekter. Det kan være nyttig for å sikre at en fil kan åpnes, eller for å kontrollere om en fil allerede er lagret på riktig sted.

## Hvordan

For å sjekke om en mappe eksisterer i et Rust-program, kan du bruke følgende kode:

```Rust
use std::fs;

let directory = "/hjem/mappe";

if fs::metadata(directory).is_ok() {
    println!("Mappen eksisterer!");
} else {
    println!("Mappen eksisterer ikke.");
}
```

I dette eksempelet bruker vi funksjonen `fs::metadata()` som returnerer en `std::fs::Metadata`-struktur for gitt filbane. Vi bruker så `is_ok()`-metoden for å sjekke om det ikke har skjedd en feil.

## Dypdykk

Når du sjekker om en mappe eksisterer, er det viktig å være oppmerksom på at mappen kan være skjult, eller at du ikke har tillatelse til å lese den. I disse tilfellene vil `fs::metadata()`-funksjonen returnere en feil. For å håndtere dette kan du bruke et match-uttrykk, som lar deg håndtere både suksess og feiltilfeller.

```Rust
match fs::metadata(directory) {
    Ok(metadata) => println!("Mappen eksisterer!"),
    Err(error) => println!("Kunne ikke sjekke om mappen eksisterer: {}", error),
}
```

Du kan også bruke `fs::symlink_metadata()`-funksjonen hvis du vil sjekke metadata for en symbolisk lenke i stedet for den endelige destinasjonsfilen.

## Se også

Følgende lenker kan være nyttige for å lære mer om mapping av filer og mapper i Rust:

- [Offisiell dokumentasjon for `std::fs`](https://doc.rust-lang.org/std/fs/index.html)
- [TutorialsPoint om lesing og skriving av filer](https://www.tutorialspoint.com/rust/rust_file_handling.htm)
- [StackOverflow-tråd om å sjekke om en mappe eksisterer](https://stackoverflow.com/questions/28392008/how-to-use-rust-code-to-create-a-folder-if-it-does-not-exist)

Vi håper denne artikkelen var nyttig for å hjelpe deg med å sjekke om mapper eksisterer i Rust!