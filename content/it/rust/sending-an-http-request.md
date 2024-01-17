---
title:                "Inviare una richiesta http"
html_title:           "Rust: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si invia una richiesta HTTP?

Invio di una richiesta HTTP è l'azione di inviare una richiesta da un client (come un browser web) a un server. I programmatori lo fanno per interagire con una risorsa o un servizio disponibile su un server, come una pagina web o un'API. 

## Come fare:

```Rust
use reqwest;
use std::collections::HashMap;


// Esempio di richiesta GET
let response = reqwest::get("https://jsonplaceholder.typicode.com/posts/1")
    .await?;

if response.status().is_success() {
    let body = response.text().await?;
    println!("Corpo della risposta: {}", body);
}

// Esempio di richiesta POST con parametri
let params = [("username", "john"), ("password", "secret")];
let client = reqwest::Client::new();
let response = client.post("https://jsonplaceholder.typicode.com/posts")
    .form(&params)
    .send()
    .await?;

if response.status().is_success() {
    let body = response.text().await?;
    println!("Corpo della risposta: {}", body);
}
```

Output:

Corpo della risposta: {
  "userId": 1,
  "id": 1,
  "title": "titolo",
  "body": "testo della pagina"
}

Corpo della risposta: {
  "username": "john",
  "password": "secret",
  "id": 101
}

## Approfondimento:

Mandare richieste HTTP è un'azione fondamentale della programmazione web. Originariamente, il protocollo HTTP è stato creato nel 1991 da Tim Berners-Lee ed è stato adottato come standard per scambiare informazioni su Internet. Esistono anche alternative a HTTP, come HTTPS che utilizza la crittografia per proteggere le comunicazioni.

Una implementazione comune di invio di richieste HTTP in Rust è attraverso la libreria reqwest. Tuttavia, ci sono anche altre librerie disponibili come hyper e actix-web. 

## Vedi anche:

- [Documentazione ufficiale di Reqwest](https://docs.rs/reqwest/)
- [Tutorial su come inviare richieste HTTP con Rust](https://erwabook.com/intro/http.html)
- [Progetto di esempio di invio di richieste HTTP in Rust](https://github.com/ZcashFoundation/zecwallet-light-cli)