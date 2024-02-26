---
date: 2024-01-20 18:02:49.678860-07:00
description: "Mandare una richiesta HTTP con autenticazione di base significa inserire\
  \ username e password per accedere a risorse protette. I programmatori lo fanno\
  \ per\u2026"
lastmod: '2024-02-25T18:49:41.093213-07:00'
model: gpt-4-1106-preview
summary: "Mandare una richiesta HTTP con autenticazione di base significa inserire\
  \ username e password per accedere a risorse protette. I programmatori lo fanno\
  \ per\u2026"
title: Inviare una richiesta http con autenticazione di base
---

{{< edit_this_page >}}

## What & Why?
Mandare una richiesta HTTP con autenticazione di base significa inserire username e password per accedere a risorse protette. I programmatori lo fanno per interagire con API che richiedono sicurezza.

## How to:
Installiamo `reqwest`, una crate Rust per effettuare chiamate HTTP.

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Usiamo `reqwest` con autenticazione di base in un ambiente asincrono:

```rust
use reqwest::header::{HeaderMap, AUTHORIZATION, CONTENT_TYPE};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let username = "user";
    let password = "pass";
    let auth_value = format!("Basic {}", base64::encode(format!("{}:{}", username, password)));

    let mut headers = HeaderMap::new();
    headers.insert(AUTHORIZATION, auth_value.parse().unwrap());
    headers.insert(CONTENT_TYPE, "application/json".parse().unwrap());

    let response = client
        .get("http://example.com/resource")
        .headers(headers)
        .send()
        .await?;

    println!("Risposta: {}", response.text().await?);
    Ok(())
}
```

Risultato campione:

```
Risposta: {"status": "success", "data": ...}
```

## Deep Dive
L'autenticazione di base HTTP, definita nel RFC 7617, codifica `username:password` con Base64. Alternativa più sicura: autenticazione Bearer con token. Implementarla con `reqwest` è semplice, ma attenzione, Base64 non è crittografia, quindi HTTPS è essenziale per proteggere le credenziali. `reqwest` gestisce internamente dettagli come la gestione della connessione e il riutilizzo.

## See Also
- Documentazione di `reqwest`: https://docs.rs/reqwest/
- RFC 7617 (Autenticazione di base HTTP): https://tools.ietf.org/html/rfc7617
- Guida per HTTP su MDN (Mozilla Developer Network): https://developer.mozilla.org/en-US/docs/Web/HTTP
