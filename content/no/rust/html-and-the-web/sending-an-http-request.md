---
date: 2024-01-20 18:00:47.775010-07:00
description: "En HTTP-foresp\xF8rsel lar oss snakke med en webserver. Vi gj\xF8r dette\
  \ for \xE5 hente data, sende informasjon, eller starte handlinger p\xE5 nett."
lastmod: '2024-03-11T00:14:14.105385-06:00'
model: gpt-4-1106-preview
summary: "En HTTP-foresp\xF8rsel lar oss snakke med en webserver. Vi gj\xF8r dette\
  \ for \xE5 hente data, sende informasjon, eller starte handlinger p\xE5 nett."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-forespørsel lar oss snakke med en webserver. Vi gjør dette for å hente data, sende informasjon, eller starte handlinger på nett.

## Slik gjør du:
Rust har ikke innebygget støtte for å sende HTTP-forespørsler, men `reqwest`-biblioteket gjør jobben enkel. Først, inkluder det i `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
```

Nå kan du sende en GET-forespørsel og håndtere responsen slik:

```rust
use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let response = reqwest::get("https://www.example.com").await?;
    
    println!("Status for vår henvendelse: {}", response.status());
    println!("Innholdet: {}", response.text().await?);

    Ok(())
}
```

Dette vil gi deg statuskoden og innholdet fra `example.com`.

## Dypdykk
Før i tiden, brukte vi biblioteket `hyper` for lavnivå HTTP-operasjoner i Rust. `reqwest` bygger videre på `hyper`, men gir en høyere-nivå API som er lettere å bruke. Alternativer inkluderer `ureq` eller `surf`, avhengig av behovet for asynkronitet og andre funksjoner.

`reqwest` støtter asynkrone operasjoner med `tokio`-runtime. Dette er viktig for å skrive moderne, effektive applikasjoner som kan utføre flere oppgaver parallelt.

Når du sender en HTTP-forespørsel, bygger klienten en komplett HTTP-versjon av meldingen, inkludert headers og payload, og utfører et nettverkskall for å sende den til serveren. Responsen blir deretter behandlet av klienten.

## Se også:
- [Reqwest Documentation](https://docs.rs/reqwest)
- [Official Rust Book](https://doc.rust-lang.org/stable/book/)
- [Async Programming in Rust with async-std](https://async.rs/)
- [Hyper HTTP Library](https://hyper.rs/)
