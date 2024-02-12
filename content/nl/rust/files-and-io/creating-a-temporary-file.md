---
title:                "Een tijdelijk bestand aanmaken"
aliases:
- /nl/rust/creating-a-temporary-file.md
date:                  2024-01-28T21:58:46.856500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tijdelijk bestand maken betekent het creëren van een kortstondig bestand voor tussenliggende verwerking. Programmeurs doen dit om gegevens op te slaan zonder de bestandsruimte van de gebruiker te vervuilen en om ervoor te zorgen dat gevoelige informatie na gebruik wordt gewist.

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
