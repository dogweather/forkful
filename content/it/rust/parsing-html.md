---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:33:53.404385-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing di HTML è il processo di analizzare il codice HTML per estrarre dati o strutturare informazioni. Programmatori lo fanno per automatizzare l'interazione con pagine web o per il web scraping, ottenendo dati da siti web per analisi o archiviazione.

## How to:
Rust è un linguaggio che pone grande enfasi sulla sicurezza e sulla performance. Per fare il parsing di HTML, possiamo utilizzare la crate `scraper` che sfrutta `html5ever` per fare il parsing in modo affidabile. Ecco un esempio:

```Rust
use scraper::{Html, Selector};

fn main() {
    let html_content = r#"
        <html>
            <body>
                <p class="message">Ciao, mondo di Rust!</p>
                <div class="author">Scritto da: Contract Author</div>
            </body>
        </html>
    "#;

    let parsed_html = Html::parse_document(html_content);
    let selector = Selector::parse(".message").unwrap();

    for element in parsed_html.select(&selector) {
        println!("Messaggio trovato: {}", element.inner_html());
    }
}
```

Questo codice stampa:

```
Messaggio trovato: Ciao, mondo di Rust!
```

## Deep Dive
Il parsing di HTML non è un concetto nuovo e Rust offre librerie moderne per eseguirlo bene. Historically, linguaggi come Python con BeautifulSoup hanno dominato il campo, ma Rust presenta vantaggi in termini di prestazioni e sicurezza grazie al suo sistema di tipi e ownership.

Altri crate di Rust per il parsing di HTML includono `html5ever` direttamente, che è la stessa libreria usata da `scraper` sotto il cofano, ma con più controllo sul parsing a basso livello. `scraper` invece offre una API di alto livello ispirata a jQuery che è più amichevole per l'uso quotidiano.

Implementare il parsing di HTML con `scraper` è semplice:
1. Si crea un oggetto `Html` per fare il parsing del contenuto HTML.
2. Si crea un `Selector` per identificare gli elementi di interesse nel documento.
3. Si esegue la selezione e si estraggono i dati desiderati.

## See Also
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/): una collezione di semplici esempi per fare varie attività in Rust.