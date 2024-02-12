---
title:                "Analisi del HTML"
aliases:
- /it/rust/parsing-html.md
date:                  2024-02-03T19:12:53.217281-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisi del HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

L'analisi di HTML in Rust è il processo di estrazione dati da documenti HTML, essenziale per il web scraping, l'estrazione di dati o la costruzione di web crawler. I programmatori fanno ciò per automatizzare la raccolta di informazioni dal web, analizzare i contenuti web o migrare contenuti da una piattaforma all'altra.

## Come fare:

Per analizzare HTML in Rust, si utilizza spesso il crate `scraper`, che fornisce un'interfaccia di alto livello per attraversare e manipolare documenti HTML.

Prima, aggiungi `scraper` al tuo `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Di seguito, ecco un semplice esempio che estrae tutti gli URL dei link da una stringa HTML data:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Link 1</a>
        <a href="http://example.com/2">Link 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Trovato link: {}", link);
    }
}
```

Output:

```
Trovato link: http://example.com/1
Trovato link: http://example.com/2
```

In questo esempio, analizziamo un semplice documento HTML per trovare tutti gli elementi `<a>` ed estrarne gli attributi `href`, stampando effettivamente gli URL di tutti i link nel documento. La libreria `scraper` semplifica l'analisi di HTML e la selezione di elementi specifici usando i selettori CSS, rendendola una risorsa ideale per compiti di web scraping in Rust.
