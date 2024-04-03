---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:38.037036-07:00
description: "\xC5 skrive til standard feil (stderr) i Rust handler om \xE5 dirigere\
  \ feilmeldinger og diagnostikk til konsollen separat fra standard utdata (stdout).\u2026"
lastmod: '2024-03-13T22:44:40.590261-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standard feil (stderr) i Rust handler om \xE5 dirigere feilmeldinger\
  \ og diagnostikk til konsollen separat fra standard utdata (stdout)."
title: Skriving til standardfeil
weight: 25
---

## Hva & hvorfor?
Å skrive til standard feil (stderr) i Rust handler om å dirigere feilmeldinger og diagnostikk til konsollen separat fra standard utdata (stdout). Programmerere gjør dette for å skille normalt programutdata fra feilmeldinger, noe som gjør det lettere å håndtere feil på en passende måte eller omdirigere dem til logger eller filer under utførelse.

## Hvordan gjøre det:
Rust gir en enkel måte å skrive til stderr på ved bruk av `eprintln!`-makroen, likt som hvordan `println!` brukes for stdout. Her er et grunnleggende eksempel:

```rust
fn main() {
    eprintln!("Dette er en feilmelding!");
}
```

Eksempelutdata (til standard feil):
```
Dette er en feilmelding!
```

For mer kontroll over feilmeldingene, som når du vil formatere tekst eller håndtere I/O-resultater, bruk `stderr`-funksjonen fra `std::io`-modulen. Denne metoden gir et håndtak til den globale stderr-strømmen, som du deretter kan skrive til ved å bruke metoder som `write_all` eller `writeln` fra `Write`-trekket:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut håndtak = stderr.lock();
    
    writeln!(håndtak, "Formatert feilmelding: {}", 404).expect("Klarte ikke å skrive til stderr");
}
```

Eksempelutdata (til standard feil):
```
Formatert feilmelding: 404
```

Hvis du jobber i miljøer eller applikasjoner der du er avhengig av biblioteker for logging eller feilhåndtering, er biblioteker som `log` og `env_logger` populære. Selv om de brukes mer for loggingsformål, er de konfigurerbare og kan dirigere feilloggnivåer til stderr. Nedenfor er et enkelt brukseksempel med `log` og `env_logger`:

Først, legg til avhengighetene i din `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Deretter, sett opp og bruk loggingen i applikasjonen din:
```rust
fn main() {
    env_logger::init();
    log::error!("Dette er en feilmelding logget til stderr");
}
```

Å kjøre dette programmet (etter å ha satt opp `env_logger` med en passende miljøvariabel, for eksempel `RUST_LOG=error`) vil skrive ut feilmeldingen til stderr, ved å bruke loggingsinfrastrukturen.

```plaintext
FEIL: Dette er en feilmelding logget til stderr
```
