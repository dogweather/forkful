---
date: 2024-01-20 17:41:21.850588-07:00
description: "Slik gj\xF8r du det: Samplereksempelet viser hvordan du oppretter en\
  \ midlertidig fil, skriver til den, og bekrefter opprettelsen. Husk \xE5 legge til\
  \ `tempfile`\u2026"
lastmod: '2024-04-05T21:53:41.571969-06:00'
model: gpt-4-1106-preview
summary: Samplereksempelet viser hvordan du oppretter en midlertidig fil, skriver
  til den, og bekrefter opprettelsen.
title: Opprette en midlertidig fil
weight: 21
---

## Slik gjør du det:
```rust
use std::fs::File;
use std::io::{self, Write};
use tempfile::Builder;

fn main() -> io::Result<()> {
    let mut tmpfile = Builder::new()
        .prefix("eksempel")
        .suffix(".tmp")
        .tempfile_in("/tmp")?
        .into_file();
    
    writeln!(tmpfile, "Hei fra Rust!")?;
    // Filen eksisterer for øyeblikket og har noe innhold
    println!("Midlertidig fil opprettet.");

    Ok(())
}
```
Samplereksempelet viser hvordan du oppretter en midlertidig fil, skriver til den, og bekrefter opprettelsen. Husk å legge til `tempfile` crate i din `Cargo.toml` før du bruker koden.

## Dypdykk
Historisk sett har midlertidige filer blitt brukt i programmering for å håndtere data mellom forskjellige operasjoner eller sessioner. De er spesielt nyttige i nettsteder og serverapplikasjoner der mange brukere trenger sin egen unike, men kortvarige, lagringsplass. Utover `tempfile`-biblioteket som vist i eksempelet over, kan midlertidige filer i Rust også lages med standard bibliotekets `TempDir` for å lage en midlertidig katalog. 

Implementeringsdetaljer inkluderer håndtering av tilfeldig navngivning for å unngå navnekollisjon, automatisk sletting av filer etter bruk, og valg av riktig lokasjon for midlertidige filer basert på operativsystemets konvensjoner. 

Alternativer til midlertidige filer kan være in-memory databaser som Redis, eller bruk av datastrukturer som buffere, avhengig av applikasjonens krav og kompleksiteten av datamanipulering som er nødvendig.

## Se Også
- Rust's tempfile crate dokumentasjon: https://docs.rs/tempfile/latest/tempfile/
- Standard bibliotekets I/O-module: https://doc.rust-lang.org/std/io/
- Benchmarking av fil-I/O i Rust: https://nnethercote.github.io/perf-book/io.html
