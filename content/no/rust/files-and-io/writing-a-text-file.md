---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:24.983286-07:00
description: "Hvordan: Rusts standardbibliotek tilbyr robuste verkt\xF8y for filmanipulering,\
  \ hovedsakelig innkapslet innenfor `std::fs` og `std::io`-modulene. Her er et\u2026"
lastmod: '2024-03-13T22:44:40.592200-06:00'
model: gpt-4-0125-preview
summary: "Rusts standardbibliotek tilbyr robuste verkt\xF8y for filmanipulering, hovedsakelig\
  \ innkapslet innenfor `std::fs` og `std::io`-modulene."
title: Skrive en tekstfil
weight: 24
---

## Hvordan:
Rusts standardbibliotek tilbyr robuste verktøy for filmanipulering, hovedsakelig innkapslet innenfor `std::fs` og `std::io`-modulene. Her er et enkelt eksempel for å opprette og skrive til en tekstfil:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

Etter å ha kjørt denne koden, vil du finne en fil med navnet "hello.txt" med innholdet "Hello, world!".

For mer komplekse scenarioer, som å legge til tekst i en eksisterende fil eller håndtere større data effektivt, tilbyr Rust ekstra funksjonalitet. Her er hvordan du legger til tekst i en eksisterende fil:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Legger til mer tekst.")?;
    Ok(())
}
```

Å kjøre dette vil legge til " Legger til mer tekst." på slutten av `hello.txt`.

I noen tilfeller kan bruk av tredjepartsbiblioteker forenkle filoperasjoner. `serde`-kassen, kombinert med `serde_json`, for eksempel, tillater serialisering og deserialisering av datastrukturer til og fra JSON-format, og tilbyr en høy-nivå tilnærming til å skrive filer:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

Etter å ha kjørt koden ovenfor, vil `user.json` inneholde en JSON-representasjon av `User`-strukturen. Merk at bruk av `serde` og `serde_json` krever at du legger til disse kassene i din `Cargo.toml`.

Å skrive tekstfiler i Rust, enten gjennom standardbiblioteket eller med hjelp av eksterne kasser, er en enkel, men kraftfull måte å håndtere dataoppbevaring i applikasjonene dine på.
