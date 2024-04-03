---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:12.308327-07:00
description: "Hoe: Laten we een webpagina downloaden met Rust's `reqwest` crate, die\
  \ een eenvoudige, asynchrone API biedt voor het maken van HTTP-verzoeken. Voeg eerst\u2026"
lastmod: '2024-03-13T22:44:50.592891-06:00'
model: gpt-4-0125-preview
summary: Laten we een webpagina downloaden met Rust's `reqwest` crate, die een eenvoudige,
  asynchrone API biedt voor het maken van HTTP-verzoeken.
title: Een webpagina downloaden
weight: 42
---

## Hoe:
Laten we een webpagina downloaden met Rust's `reqwest` crate, die een eenvoudige, asynchrone API biedt voor het maken van HTTP-verzoeken.

Voeg eerst `reqwest` en `tokio` toe aan je `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { versie = "1", features = ["full"] }
```

Nu, in je Rust-code:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let lichaam = res.text().await?;
    println!("Lichaam:\n{}", lichaam);

    Ok(())
}
```

Een voorbeelduitvoer kan er zo uitzien, hoewel de daadwerkelijke inhoud kan variëren:

```
Lichaam:
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</body>
</html>
```

## Diepere Duik
De `reqwest` crate is een van de meest eenvoudige manieren om webinhoud te downloaden in Rust. Het is geëvolueerd uit eerdere HTTP-bibliotheken, en biedt zowel synchrone als asynchrone interfaces.

Alternatieven zijn onder andere lager niveau bibliotheken zoals `hyper` (die `reqwest` zelf onder de motorkap gebruikt), of het gebruik van `curl` bindings voor Rust.

Belangrijke implementatiestappen voor het downloaden van een pagina zijn het maken van een HTTP GET-verzoek en het verwerken van de reactie. Asynchroon programmeren met `tokio` betekent dat je app responsief blijft terwijl de netwerkoperatie wordt voltooid.

## Zie Ook:
- [`reqwest` documentatie](https://docs.rs/reqwest/)
- [`tokio` documentatie](https://docs.rs/tokio/)
- [Rust `async`/`await` boek](https://rust-lang.github.io/async-book/)
- [MDN webdocumentatie over HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
