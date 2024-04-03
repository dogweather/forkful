---
date: 2024-01-20 17:44:40.119153-07:00
description: "Scaricare una pagina web significa prelevare il contenuto che normalmente\
  \ vediamo nel browser e salvarlo localmente. I programmatori fanno questo per\u2026"
lastmod: '2024-03-13T22:44:43.216907-06:00'
model: gpt-4-1106-preview
summary: Scaricare una pagina web significa prelevare il contenuto che normalmente
  vediamo nel browser e salvarlo localmente.
title: Scaricare una pagina web
weight: 42
---

## What & Why?
Scaricare una pagina web significa prelevare il contenuto che normalmente vediamo nel browser e salvarlo localmente. I programmatori fanno questo per processare o analizzare i dati, testare siti o creare applicazioni che interagiscono con il web.

## How to:
Per scaricare una pagina web in Rust, useremo il pacchetto `reqwest`, molto popolare e con molte funzionalità. Prima di tutto, includi `reqwest` nel tuo `Cargo.toml`.

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Poi esegui il download della pagina:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let response = reqwest::get(url).await?;

    let body = response.text().await?;
    println!("Body:\n{}", body);
    
    Ok(())
}
```

Esempio di output:

```plaintext
Body:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## Deep Dive
Nel passato, librerie come `hyper` erano il modo diretto per fare richieste HTTP in Rust. Ora, `reqwest` si appoggia su `hyper` e offre un'API più semplice. Per gestire le operazioni asincrone, `tokio` è il runtime più usato.

Ci sono anche alternative non asincrone come `ureq` se il tuo progetto non richiede asincronia. `reqwest` stesso supporta anche un client bloccante.

Quando scarichi una pagina, il handling degli errori e degli status HTTP è cruciale. Usa `response.status()` per controllare lo status e gestire diversi scenari (redirection, client error, server error).

## See Also
- Documentazione di `reqwest`: https://docs.rs/reqwest
- Il libro asincrono di Rust (Asynchronous Programming in Rust): https://rust-lang.github.io/async-book/
- `hyper` crate: https://crates.io/crates/hyper
- `tokio` runtime: https://tokio.rs/
- `ureq`, un client HTTP bloccante: https://crates.io/crates/ureq
