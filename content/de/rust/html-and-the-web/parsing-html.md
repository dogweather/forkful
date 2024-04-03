---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:05.342740-07:00
description: "Wie: Um HTML in Rust zu parsen, wirst du oft das `scraper`-Crate verwenden,\
  \ das eine High-Level-Schnittstelle bietet, um HTML-Dokumente zu durchlaufen und\u2026"
lastmod: '2024-03-13T22:44:53.670678-06:00'
model: gpt-4-0125-preview
summary: Um HTML in Rust zu parsen, wirst du oft das `scraper`-Crate verwenden, das
  eine High-Level-Schnittstelle bietet, um HTML-Dokumente zu durchlaufen und zu manipulieren.
title: HTML parsen
weight: 43
---

## Wie:
Um HTML in Rust zu parsen, wirst du oft das `scraper`-Crate verwenden, das eine High-Level-Schnittstelle bietet, um HTML-Dokumente zu durchlaufen und zu manipulieren.

Füge zunächst `scraper` deiner `Cargo.toml` hinzu:

```toml
[dependencies]
scraper = "0.12.0"
```

Im Folgenden findest du ein einfaches Beispiel, das alle Link-URLs aus einem gegebenen HTML-String extrahiert:

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
        println!("Gefundener Link: {}", link);
    }
}
```

Ausgabe:

```
Gefundener Link: http://example.com/1
Gefundener Link: http://example.com/2
```

In diesem Beispiel parsen wir ein einfaches HTML-Dokument, um alle `<a>`-Elemente zu finden und deren `href`-Attribute zu extrahieren, wodurch effektiv die URLs aller Links im Dokument ausgegeben werden. Die `scraper`-Bibliothek vereinfacht das Parsen von HTML und das Auswählen spezifischer Elemente mit CSS-Selektoren, was sie zu einer bevorzugten Wahl für Web-Scraping-Aufgaben in Rust macht.
