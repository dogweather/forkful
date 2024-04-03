---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:59.462130-07:00
description: "Jak to zrobi\u0107: Aby parsowa\u0107 HTML w Rust, cz\u0119sto u\u017C\
  ywa si\u0119 pakietu `scraper`, kt\xF3ry dostarcza wysokopoziomowy interfejs do\
  \ przegl\u0105dania i manipulowania\u2026"
lastmod: '2024-03-13T22:44:35.182113-06:00'
model: gpt-4-0125-preview
summary: "Aby parsowa\u0107 HTML w Rust, cz\u0119sto u\u017Cywa si\u0119 pakietu `scraper`,\
  \ kt\xF3ry dostarcza wysokopoziomowy interfejs do przegl\u0105dania i manipulowania\
  \ dokumentami HTML."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
Aby parsować HTML w Rust, często używa się pakietu `scraper`, który dostarcza wysokopoziomowy interfejs do przeglądania i manipulowania dokumentami HTML.

Najpierw dodaj `scraper` do swojego `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Następnie, oto prosty przykład, który wydobywa wszystkie adresy URL linków z danego ciągu HTML:

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
        println!("Znaleziono link: {}", link);
    }
}
```

Wyjście:

```
Znaleziono link: http://example.com/1
Znaleziono link: http://example.com/2
```

W tym przykładzie parsujemy prosty dokument HTML, aby znaleźć wszystkie elementy `<a>` i wydobyć ich atrybuty `href`, skutecznie wypisując adresy URL wszystkich linków w dokumencie. Biblioteka `scraper` upraszcza parsowanie HTML i selekcjonowanie konkretnych elementów za pomocą selektorów CSS, czyniąc ją dobrym wyborem do zadań związanych z web scrapingiem w Rust.
