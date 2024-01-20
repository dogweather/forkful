---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Invio di una richiesta HTTP è un'esigenza fondamentale in molte applicazioni web; permette la comunicazione tra client e server. I programmatori lo fanno per interagire con API, scaricare file, e così via.

## Come si fa:

Utilizzeremo il pacchetto `reqwest` per fare richieste HTTP, quindi prima dovrai installarlo.

Aggiungi questa riga al tuo `Cargo.toml` sotto la sezione `[dependencies]`.

```Rust
reqwest = "0.11"
```

Esempio di invio di una richiesta GET:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://httpbin.org/ip").await?;

    println!("{}", response.text().await?);
    Ok(())
}
```
Risposta prevista:

```Rust
{
  "origin": "123.45.67.89"
}
```

## Approfondimento

L'invio di richieste HTTP è stato a lungo un componente del web, originariamente definito nel 1991. Nel contesto di Rust, un'alternativa a `reqwest` può essere l'uso di `hyper`, una libreria HTTP piuttosto basso livello. I dettagli implementativi dell'invio di richieste HTTP coinvolgono la creazione di un client, la definizione di un URL e l'opzionale invio di dati al server.

## Vedi Anche:

* Documentazione `reqwest`: https://docs.rs/reqwest/0.11.3/reqwest/
* Per saperne di più sull'HTTP: https://it.wikipedia.org/wiki/Hypertext_Transfer_Protocol
* Documentazione `hyper`: https://docs.rs/hyper/0.14.4/hyper/