---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:46.856500-07:00
description: 'Hoe: In Rust is de `tempfile` crate een goede vriend voor tijdelijke
  bestandsgeintjes. Voeg het toe aan je `Cargo.toml`.'
lastmod: '2024-03-13T22:44:50.613564-06:00'
model: gpt-4-0125-preview
summary: In Rust is de `tempfile` crate een goede vriend voor tijdelijke bestandsgeintjes.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
In Rust is de `tempfile` crate een goede vriend voor tijdelijke bestandsgeintjes. Voeg het toe aan je `Cargo.toml`:

```toml
[dependencies]
tempfile = "3.3.0"
```

Vervolgens kun je zo een tijdelijk bestand creëren:

```rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    write!(temp_file, "Hallo, wereld!")?;

    let mut inhoud = String::new();
    temp_file.reopen()?.read_to_string(&mut inhoud)?;
    println!("Tijdelijk bestand bevat: {}", inhoud);

    // Tijdelijk bestand wordt hier verwijderd wanneer `temp_file` buiten gebruik vervalt
    Ok(())
}
```

Voer de code uit. Magie gebeurt. Een bestand verschijnt, en dan poef—weg wanneer je klaar bent.

## Diepe Duik
Historisch gezien zijn tijdelijke bestanden in de computerwereld zo oud als de weg naar Rome. Ze zijn altijd een eenvoudige maar effectieve manier geweest om gegevens te verwerken die geen langdurige opslag nodig hebben. In de wereld van Rust strijkt de `tempfile` crate het proces van tijdelijke bestanden glad, door bestanden automatisch op te ruimen wanneer ze niet meer nodig zijn, waardoor de oude hoofdpijn van handmatige opruiming wordt vermeden.

Alternatieven? Zeker, je zou je eigen oplossing kunnen uitrollen met `std::fs` en handmatige opruiming, maar waarom het wiel opnieuw uitvinden?

Wat betreft details? `tempfile` maakt bestanden in de door het besturingssysteem aangewezen tijdelijke map, en bestandsnamen worden door elkaar gehaald om botsingen te voorkomen en de veiligheid te verhogen.

## Zie Ook
- Rust `tempfile` documentatie: [https://docs.rs/tempfile/](https://docs.rs/tempfile/)
- Rust standaardbibliotheek I/O: [https://doc.rust-lang.org/std/io/](https://doc.rust-lang.org/std/io/)
