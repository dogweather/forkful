---
title:                "Een webpagina downloaden"
aliases: - /nl/rust/downloading-a-web-page.md
date:                  2024-01-28T21:59:12.308327-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het verzamelen van de gegevens die het bevat. Programmeurs doen dit om informatie te verkrijgen, tests te automatiseren, data te scrapen, of de beschikbaarheid van de site te controleren.

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
