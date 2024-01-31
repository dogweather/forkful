---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:34:03.643944-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie HTML to proces analizy kodu HTML i przekształcania go w użyteczne struktury danych. Programiści to robią, by manipulować i korzystać z zawartości stron internetowych – ekstrakcja danych, testy aplikacji webowych, itp.

## Jak to zrobić?
Użyjemy crate'a `scraper` – to Rustowa biblioteka do parsowania HTML.

```rust
use scraper::{Html, Selector};

fn main() {
    let html_content = r#"
        <html>
            <body>
                <p class='greeting'>Cześć, Rustaceans!</p>
            </body>
        </html>
    "#;

    let document = Html::parse_document(html_content);
    let selector = Selector::parse(".greeting").unwrap();

    for element in document.select(&selector) {
        println!("Znaleziony tekst: {}", element.inner_html());
    }
}
```

Wynik działania:
```
Znaleziony tekst: Cześć, Rustaceans!
```

## Wgłębiamy się
Parsowanie HTML w Rust zaczęło się od prostych parserów, ale z czasem ewoluowało do bardziej wyrafinowanych bibliotek, takich jak `scraper`. Zamiast `scraper`, można użyć `select` lub `html5ever`, jeśli szukasz większej kontroli lub wydajności.

Głębokość parsowania HTML zależy od potrzeb: można szukać tylko określonych tagów, atrybutów czy klas, lub przechodzić przez cały dokument. Implementacja w `scraper` polega na używaniu selektorów CSS do lokalizowania danych, które chcemy wyciągnąć.

## Zobacz również
- [Dokumentacja crate'a `scraper`](https://docs.rs/scraper/)
- [Rust Cookbook - Parsing HTML](https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html)
