---
aliases:
- /nl/rust/sending-an-http-request/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:40.018860-07:00
description: "Het versturen van een HTTP-verzoek haalt gegevens op van of verzendt\
  \ gegevens naar een webserver. Programmeurs doen dit om te interacteren met webdiensten\u2026"
lastmod: 2024-02-18 23:09:01.615714
model: gpt-4-0125-preview
summary: "Het versturen van een HTTP-verzoek haalt gegevens op van of verzendt gegevens\
  \ naar een webserver. Programmeurs doen dit om te interacteren met webdiensten\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Het versturen van een HTTP-verzoek haalt gegevens op van of verzendt gegevens naar een webserver. Programmeurs doen dit om te interacteren met webdiensten of API's – informatie ophalen, updates plaatsen, noem maar op.

## Hoe:
Om een GET-verzoek in Rust te versturen, gebruiken we de `reqwest` crate. Voeg deze eerst toe aan je `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Roer nu wat asynchrone Rust-code op:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response_text = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    
    println!("Reactie: {}", response_text);
    Ok(())
}
```

Een voorbeelduitvoer kan er zo uitzien:

```
Reactie: {"sleutel": "waarde", "hallo": "wereld"}
```

Dit is alles wat nodig is om een eindpunt te raken met een GET-verzoek!

## Diepe Duik
HTTP-verzoeken zijn zo oud als de weg naar Rome in internetjaren. Ze vormen de ruggengraat van webgebaseerde communicatie. Rust gebruikt crates zoals `reqwest` omdat het geen web-specifieke taal is – flexibiliteit is cruciaal. `reqwest` is gebouwd op `hyper`, wat snel en low-level is, maar `reqwest` voegt daar gebruiksgemak aan toe.

Alternatieven voor `reqwest`? Zeker. `hyper` voor de snelheidsduivels, `surf` als je van asynchrone Rust houdt of `ureq` voor eenvoud – geen asynchroon gedoe nodig.

Onder de motorkap, wanneer je een HTTP-verzoek verzendt, doet Rust veel wat elke taal zou doen: een TCP-verbinding tot stand brengen, een geformatteerd HTTP-verzoek verzenden en de ruwe reactie interpreteren. Asynchrone afhandeling van deze verzoeken is waar Rust uitblinkt, waardoor je andere dingen kunt doen terwijl je wacht op het antwoord van de server.

## Zie Ook
- [reqwest Documentatie](https://docs.rs/reqwest/)
- [The Rust Async Book](https://rust-lang.github.io/async-book/)
- [Hyper HTTP Bibliotheek](https://hyper.rs/)
- [API Richtlijnen](https://rust-lang.github.io/api-guidelines/)
