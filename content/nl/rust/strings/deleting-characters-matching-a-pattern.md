---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:02.878533-07:00
description: 'Hoe te doen: In Rust kunnen we de `replace` methode van het `String`
  type of regex gebruiken voor meer complexe patronen. Hier is hoe je dat doet.'
lastmod: '2024-03-13T22:44:50.578039-06:00'
model: gpt-4-0125-preview
summary: In Rust kunnen we de `replace` methode van het `String` type of regex gebruiken
  voor meer complexe patronen.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe te doen:
In Rust kunnen we de `replace` methode van het `String` type of regex gebruiken voor meer complexe patronen. Hier is hoe je dat doet:

```rust
fn main() {
    let phrase = "Hallo, _wereld_! -- Programmeren in Rust --".to_string();
    // Vervang underscores met niets
    let cleaned = phrase.replace("_", "");
    println!("{}", cleaned);

    // Regex gebruiken voor meer complexe patronen (vergeet niet de regex crate toe te voegen aan Cargo.toml)
    let regex = regex::Regex::new(r"--.*?--").unwrap();
    let s = regex.replace_all(&cleaned, "");
    println!("{}", s);
}

// Uitvoer:
// Hallo, wereld! -- Programmeren in Rust --
// Hallo, wereld!
```

## Diepgaand
Het verwijderen van karakters die een patroon matchen is niet uniek voor Rust; het is een veelvoorkomende operatie in veel programmeertalen. Historisch gezien werden tools zoals `sed` in Unix gebruikt om tekst op krachtige manieren te transformeren, en nu bieden talen ingebouwde functies voor tekstmanipulatie.

In Rust is de standaard aanpak het gebruik van `replace` voor eenvoudige, vaste patronen. Voor wildcards, herhalingen, of conditionele verwijdering, wenden we ons tot regex. De regex crate is het de facto hulpmiddel hiervoor, maar onthoud, regex operaties zijn duurder in termen van prestaties, dus gebruik ze met mate.

Rust's veiligheidsgaranties strekken zich uit tot tekstverwerking. Terwijl bij sommige talen tekstmanipulatie een bron kan zijn van beveiligingskwetsbaarheden zoals buffer overflows, beschermt Rust's ontwerp tegen dergelijke problemen.

## Zie Ook
- De Rust `String` documentatie: https://doc.rust-lang.org/std/string/struct.String.html 
- `regex` crate documentatie: https://docs.rs/regex/
- Rust Regex Boek: https://rust-lang-nursery.github.io/regex/
