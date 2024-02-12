---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
aliases:
- no/rust/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:49.709627-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-forespørsel med basisgodkjenning lar et program be om data fra en server ved å inkludere brukernavn og passord i forespørselen. Programmerere bruker dette for sikker tilgang til webtjenester som krever autentisering.

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
