---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:30.281347-07:00
description: "\xC5 kapitalisere en streng i Rust inneb\xE6rer \xE5 endre strengen\
  \ slik at dens f\xF8rste bokstav er stor hvis det er en bokstav, mens resten av\
  \ strengen forblir\u2026"
lastmod: '2024-03-13T22:44:40.557273-06:00'
model: gpt-4-0125-preview
summary: "\xC5 kapitalisere en streng i Rust inneb\xE6rer \xE5 endre strengen slik\
  \ at dens f\xF8rste bokstav er stor hvis det er en bokstav, mens resten av strengen\
  \ forblir uendret."
title: Sette stor bokstav i en streng
weight: 2
---

## Hva & Hvorfor?

Å kapitalisere en streng i Rust innebærer å endre strengen slik at dens første bokstav er stor hvis det er en bokstav, mens resten av strengen forblir uendret. Programmerere utfører ofte denne operasjonen for formateringsformål, slik som å forberede ord for titler eller sikre konsekvens i brukerinput.

## Hvordan:

For å kapitalisere en streng i Rust, har du to hovedruter: å bruke standardbibliotekfunksjoner eller å bruke tredjeparts cratest for mer komplekse eller spesifikke behov. Her er hvordan du kan gjøre begge deler.

### Ved å bruke Rusts standardbibliotek

Rusts standardbibliotek tilbyr ikke en direkte metode for å kapitalisere strenger, men du kan oppnå dette ved å manipulere strengens tegn.

```rust
fn kapitaliser_første(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let min_streng = "hallo";
    println!("{}", kapitaliser_første(min_streng)); // Utdata: Hallo
}
```

### Ved å bruke `heck` Craten

For en mer rett frem tilnærming, spesielt når du arbeider innenfor en større tekstbehandlingskontekst, kan du foretrekke å bruke tredjepartsbiblioteker som `heck`. `heck` craten tilbyr ulike kasusomformingsfunksjonaliteter, inkludert en enkel måte å kapitalisere strenger på.

Først, legg til `heck` i din `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Deretter, bruk den for å kapitalisere strengen din:

```rust
extern crate heck; // Ikke nødvendig i Rust 2018 utgaven eller senere
use heck::TitleCase;

fn main() {
    let min_streng = "hallo verden";
    let kapitalisert = min_streng.to_title_case();
    println!("{}", kapitalisert); // Utdata: Hallo Verden
}
```

Merk: Metoden `to_title_case` som tilbys av `heck` kapitaliserer hvert ord i strengen, noe som kan være mer enn hva du ser etter hvis du kun ønsker den første tegnet i strengen kapitalisert. Juster bruken din i henhold til dine spesifikke behov.
