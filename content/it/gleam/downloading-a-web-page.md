---
title:                "Scaricare una pagina web"
date:                  2024-01-20T17:44:03.507449-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa prelevare i dati da un sito e salvarli localmente. I programmatori lo fanno per analizzare il contenuto, testare la disponibilità o integrare informazioni in app diverse.

## How to:
Gleam non ha una libreria standard per il networking, ma puoi usare librerie esterne come `reqwest` per Rust. Ecco un esempio di come fare:

```gleam
extern crate reqwest

fn main() {
  let body = reqwest::get("https://www.esempio.com")
               .await?
               .text()
               .await?;

  println!("Il contenuto della pagina: {:?}", body);
}
```

Ovviamente, questo è un esempio base. Nella realtà, dovrai gestire gli errori e l'asincronia correttamente.

## Deep Dive
Nel passato, scaricare una pagina web era più semplice. Oggi, con sito web dinamici e protezioni anti-bot, può essere più complicato. Librerie come `reqwest` sono evolute per gestire complessi scenari HTTP, come richieste asincrone, autenticazione e molto altro.

Alternative a `reqwest` in Gleam possono essere `hyper` per Rust o usare bindings per libcurl. Tieni presente anche che molti siti moderni offrono API RESTful per l'accesso ai loro dati, che possono essere un'opzione più semplice per alcune applicazioni.

Per il parsing del HTML scaricato, librerie come `BeautifulSoup` in Python sono popolari. In Gleam, ti potresti affidare a librerie esterne precise per Rust o Erlang per fare questo.

## See Also

- [reqwest documentation](https://docs.rs/reqwest/)
- [hyper documentation](https://hyper.rs/)
- [libcurl bindings for Rust](https://github.com/alexcrichton/curl-rust)
- [RESTful API guidelines](https://restfulapi.net/)