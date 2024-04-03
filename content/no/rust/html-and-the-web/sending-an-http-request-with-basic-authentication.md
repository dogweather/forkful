---
date: 2024-01-20 18:02:49.709627-07:00
description: "En HTTP-foresp\xF8rsel med basisgodkjenning lar et program be om data\
  \ fra en server ved \xE5 inkludere brukernavn og passord i foresp\xF8rselen. Programmerere\u2026"
lastmod: '2024-03-13T22:44:40.573571-06:00'
model: gpt-4-1106-preview
summary: "En HTTP-foresp\xF8rsel med basisgodkjenning lar et program be om data fra\
  \ en server ved \xE5 inkludere brukernavn og passord i foresp\xF8rselen."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan gjøre det:
For å sende en HTTP-forespørsel med basisgodkjenning i Rust, kan vi bruke `reqwest`-biblioteket, som forenkler prosessen. Først må du legge til `reqwest` og `base64` i cargo.toml:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13.0"
```

Deretter setter du opp koden slik:

```rust
use reqwest;
use base64;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let brukernavn = "bruker";
    let passord = "pass123";
    let godkjenningsheader = format!("Basic {}", base64::encode(format!("{}:{}", brukernavn, passord)));

    let klient = reqwest::Client::new();
    let mut headers = HashMap::new();
    headers.insert("Authorization".to_string(), godkjenningsheader);

    let respons = klient.get("https://din.api/tjeneste")
        .headers(headers)
        .send()
        .await?;

    println!("Status for svar: {}", respons.status());
    println!("Innhold av svar: {:?}", respons.text().await?);

    Ok(())
}
```

Dette sender en GET-forespørsel til 'https://din.api/tjeneste' med riktig basisgodkjenning og skriver ut svarets status og innhold.

## Dypdykk:
I tidlige dager ble HTTP-autentisering satt som en standard for å beskytte webressurser. Basisgodkjenning er en simpel metode hvor brukernavn og passord kombineres og kodes med Base64. 

Selv om denne metoden ikke er den sikreste (fordi Base64 kan dekodes lett), er den fremdeles populær for dens enkelhet. Alternativer som OAuth 2.0 tilbyr robustere sikkerhet, men krever mer kompleks implementasjon.

Ved implementering er det viktig å huske på at passordet bør ikke hardkodes. Det burde hentes fra en sikker kilde som miljøvariabler. Videre bør HTTPS brukes for å kryptere forespørselen, som garanterer at brukerdetaljene ikke er lesbare i transitt.

## Se også:
- `reqwest` dokumentasjon: https://docs.rs/reqwest/
- Rust `base64` bibliotek: https://docs.rs/base64/
- HTTP-autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- HTTPS: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview#http_is_insecure
