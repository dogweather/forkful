---
title:                "Rust: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lage midlertidige filer er en viktig del av mange programmeringsprosjekter. Det kan være nyttig å ha en midlertidig fil å jobbe med mens man utforsker og tester ideer, eller for å kunne lagre midlertidige data som ikke trenger å være permanent lagret. I Rust er det enkelt å opprette midlertidige filer, og i denne bloggposten skal vi se nærmere på hvordan det kan gjøres.

## Hvordan
For å opprette en midlertidig fil i Rust, kan man bruke standardbiblioteket `tempfile`. Først må man importere biblioteket i prosjektet sitt ved å legge til følgende linje i `Cargo.toml`-filen:

```
[dependencies]
tempfile = "3.1.0"
```

Deretter kan man begynne å bruke `tempfile` biblioteket i koden sin. Her er et eksempel på hvordan man kan opprette en midlertidig fil og skrive noe til denne:

```Rust
// Importerer tempfile biblioteket
use tempfile::tempfile;

// Oppretter en midlertidig fil
let mut temp_file = tempfile().expect("Kunne ikke opprette midlertidig fil");

// Skriver til filen
temp_file.write_all(b"Dette er en midlertidig fil").expect("Kunne ikke skrive til filen");
```

Det som er viktig å huske på når man bruker `tempfile` er at filen automatisk vil bli slettet når den blir droppet av programmet. Dette skjer for eksempel når variabelen `temp_file` går ut av scope. Man kan også selv slette filen ved å kalle `temp_file.close()` eller `temp_file.unlink()`.

## Dypdykk
En ting som er verdt å merke seg når man oppretter en midlertidig fil i Rust, er at den vil bli lagret i systemets midlertidige mappe. Den vil også få et unikt navn basert på en tilfeldig generert streng. Dette betyr at man ikke trenger å bekymre seg for å gi filen et navn eller plassere den et spesifikt sted.

En annen nyttig funksjon i `tempfile` biblioteket er muligheten til å opprette midlertidige kataloger ved å bruke `tempfile::tempdir()`. Dette kan være nyttig når man trenger et midlertidig sted å lagre flere filer.

## Se også
- [Offisiell dokumentasjon for `tempfile` biblioteket](https://docs.rs/tempfile/)
- [Rust's offisielle nettside](https://www.rust-lang.org/)

Takk for at du leste denne bloggposten om å opprette midlertidige filer i Rust. Vi håper den var nyttig og at du nå føler deg trygg på hvordan du kan bruke `tempfile` biblioteket i dine egne prosjekter. Lykke til videre med Rust-programmering!