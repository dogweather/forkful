---
title:                "Rust: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si scrive un programma Rust, abbiamo bisogno di comunicare con altri sistemi, ad esempio per recuperare dati da un API o per inviare informazioni a un server. In questi casi, è necessario inviare una richiesta HTTP per comunicare con questi altri sistemi. Ecco perché è importante sapere come inviare una richiesta HTTP in Rust.

## Come fare

Per inviare una richiesta HTTP in Rust, possiamo utilizzare la libreria `reqwest`. Per prima cosa, è necessario aggiungere la dipendenza nel file `Cargo.toml`:

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
```

Quindi, nel nostro codice, possiamo importare la libreria:

```Rust
use reqwest;
```

Per inviare una richiesta GET, dobbiamo creare un client e utilizzare il metodo `get`:

```Rust
let client = reqwest::blocking::Client::new();
let response = client.get("https://example.com").send()?;
```

Possiamo anche specificare dei parametri nella nostra richiesta, ad esempio per inviare una richiesta POST con dei dati nel body, possiamo utilizzare il metodo `post` e il builder `Form`:

```Rust
let params = [("id", "123"), ("name", "Mario")];
let response = client.post("https://example.com/users")
    .form(&params)
    .send()?;
```

Una volta inviata la richiesta, possiamo leggere la risposta utilizzando il metodo `text()` per ottenere il corpo come stringa, oppure `json()` per ottenere il corpo come oggetto JSON. Ad esempio:

```Rust
let body = response.text()?;
```

## Deep Dive

Oltre alle richieste GET e POST, la libreria `reqwest` supporta anche altre tipologie di richieste HTTP, come PUT, DELETE, PATCH e molte altre. Inoltre, possiamo anche specificare degli header personalizzati nella nostra richiesta utilizzando il builder `RequestBuilder`. Possiamo anche gestire gli errori utilizzando il costrutto `match` per controllare il codice di stato della risposta.

## Vedi anche

- [Documentazione ufficiale di Reqwest](https://docs.rs/reqwest)
- [Esempi di utilizzo di Reqwest](https://github.com/seanmonstar/reqwest/tree/master/examples)
- [Guida su come fare richieste HTTP in Rust](https://blog.logrocket.com/how-to-make-http-requests-in-rust/)