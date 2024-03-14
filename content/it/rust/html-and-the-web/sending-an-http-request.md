---
date: 2024-01-20 18:00:42.787559-07:00
description: "Mandare una richiesta HTTP \xE8 come bussare a una porta di un servizio\
  \ web per ricevere dati o inviarne. I programmatori lo fanno per interagire con\
  \ API\u2026"
lastmod: '2024-03-13T22:44:43.214873-06:00'
model: gpt-4-1106-preview
summary: "Mandare una richiesta HTTP \xE8 come bussare a una porta di un servizio\
  \ web per ricevere dati o inviarne. I programmatori lo fanno per interagire con\
  \ API\u2026"
title: Inviare una richiesta http
---

{{< edit_this_page >}}

## What & Why?
Mandare una richiesta HTTP è come bussare a una porta di un servizio web per ricevere dati o inviarne. I programmatori lo fanno per interagire con API web, scambiare dati e integrare funzionalità esterne nei loro programmi.

## How to:
Installiamo `reqwest`, una libreria HTTP di Rust. Aggiungi al tuo `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Ecco un esempio semplice per effettuare una richiesta GET:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://www.rust-lang.org")
        .await?
        .text()
        .await?;
    
    println!("Risposta del corpo: {}", response);
    Ok(())
}
```

Output:

```
Risposta del corpo: <!DOCTYPE html>...
```

## Deep Dive
Rust, con la sua forte enfasi sulla sicurezza e la concorrenza, rende il trattamento delle richieste HTTP solido e affidabile. `reqwest` si basa su `hyper`, un client HTTP più basso livello. In alternativa, si potrebbe usare `hyper` direttamente per maggiore controllo.

Altri linguaggi usano librerie simili, come `requests` in Python, ma Rust si distingue per il suo sistema di tipo e gestione dell'errore, che assicura che gestisci le risposte e gli errori correttamente.

Storicamente, Rust ha guadagnato popolarità per sistemi affidabili e attività di rete performanti. Le alternative come `curl` (tramite `curl-rust`) esistono, ma `reqwest` è ampiamente adottato per la sua interfaccia asincrona e facile adoperabilità.

## See Also
- Documentazione di `reqwest`: [https://docs.rs/reqwest/](https://docs.rs/reqwest/)
- Libro ufficiale di Rust (In inglese): [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
- Esempi `hyper`: [https://github.com/hyperium/hyper/tree/master/examples](https://github.com/hyperium/hyper/tree/master/examples)
