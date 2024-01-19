---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Scaricare una pagina web significa acquisire tutti i dati di una pagina web e salvarla localmente sul tuo dispositivo per l'uso o l'analisi. I programmatori lo fanno per vari motivi, ad esempio, per analizzare il contenuto, effettuare il web scraping, effettuare il controllo della qualità, e altro.

## Come fare:

In Rust, possiamo utilizzare la libreria `reqwest` per scaricare una pagina web.

```Rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let contents = reqwest::get("https://www.rust-lang.org")
        .await?
        .text()
        .await?;
    
    println!("{}", contents);
    Ok(())
}
```

Questo codice asincrono stampa il contenuto della pagina web all'URL https://www.rust-lang.org sulla console.

## Approfondimenti:

Prima dell'avvento di librerie come `reqwest`, il download di pagine web in Rust era un'operazione più complicata, spesso richiedeva l'utilizzo di librerie di basso livello come `hyper`. 

Un'alternativa a `reqwest` è `surf`, un'altra libreria Rust facile da utilizzare per gli HTTP client.

Nell'implementazione del download di una pagina web, `reqwest` si occupa del controllo degli errori HTTP e della gestione della connessione, rendendo l'operazione apparentemente semplice. Si basa su `hyper` per le operazioni HTTP di basso livello e `tokio` per l'asincronicità.

## Vedi anche:

- Documentazione `reqwest`: https://docs.rs/reqwest
- Documentazione `surf`: https://docs.rs/surf
- Rust Async Programming: https://rust-lang.github.io/async-book/