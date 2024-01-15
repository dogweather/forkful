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

## Perché

Se sei un programmatore che lavora con applicazioni web o sistemi distribuiti, è molto probabile che prima o poi avrai bisogno di inviare una richiesta HTTP. Queste richieste sono fondamentali quando si tratta di comunicare con server o servizi esterni, quindi è importante avere una buona comprensione di come farlo correttamente.

## Come Fare

In Rust, possiamo inviare una richiesta HTTP utilizzando la libreria `reqwest`. Per prima cosa, dobbiamo aggiungere questa dipendenza al nostro `Cargo.toml`:

```Rust
[dependencies]
reqwest = { version = "0.11.0", features = ["blocking", "json"]}
```

Ora possiamo importare la libreria nel nostro codice:

```Rust
use reqwest;
```

Una volta fatto ciò, possiamo creare una richiesta utilizzando il metodo `get()` e specificare l'URL del server a cui vogliamo inviare la richiesta:

```Rust
let response = reqwest::get("https://example.com").unwrap();
```

Per aggiungere parametri alla nostra richiesta, possiamo utilizzare il metodo `query()` e fornire una tupla contenente i parametri desiderati:

```Rust
let response = reqwest::get("https://example.com").query(&[("lang", "rust"), ("user", "me")]).unwrap();
```

Possiamo anche impostare le intestazioni della nostra richiesta utilizzando il metodo `header()`:

```Rust
let response = reqwest::get("https://example.com").header("Accept", "application/json").unwrap();
```

Una volta che abbiamo impostato tutti i parametri desiderati, possiamo inviare la richiesta utilizzando il metodo `send()` e ottenere la risposta utilizzando il metodo `text()` o `json()` a seconda del tipo di dati che ci aspettiamo nella risposta:

```Rust
let response = reqwest::get("https://example.com").send().unwrap();
let body = response.text().unwrap();
```

## Deep Dive

Ci sono molti altri metodi e opzioni disponibili nella libreria `reqwest` per inviare una richiesta HTTP. Ad esempio, possiamo impostare il timeout della nostra richiesta utilizzando il metodo `timeout()` o gestire eventuali errori utilizzando il metodo `unwrap_or()`.

Inoltre, possiamo anche utilizzare la libreria `hyper` per gestire direttamente il protocollo HTTP in modo più dettagliato, ma per la maggior parte dei casi, `reqwest` sarà sufficiente.

## Vedi Anche

- Documentazione ufficiale di `reqwest`: https://docs.rs/reqwest/
- Una guida dettagliata su HTTP in Rust: https://blog.logrocket.com/http-in-rust/
- Esempi di codice per l'invio di una richiesta HTTP utilizzando `reqwest`: https://github.com/seanmonstar/reqwest/tree/master/examples