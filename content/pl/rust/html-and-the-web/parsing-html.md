---
title:                "Analiza składniowa HTML"
date:                  2024-02-03T19:12:59.462130-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie HTML w Rust polega na wydobywaniu danych z dokumentów HTML, co jest kluczowe dla web scrapingu, ekstrakcji danych lub budowania web crawlerów. Programiści robią to, aby automatyzować zbieranie informacji z sieci, analizować zawartość webową lub migrować zawartość z jednej platformy na inną.

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