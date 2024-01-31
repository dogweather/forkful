---
title:                "Tolka HTML"
date:                  2024-01-20T15:33:59.591304-07:00
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing HTML handlar om att tolka och omvandla HTML-kod till något som programmet kan hantera och förstå. Programmerare gör detta för att manipulera, extrahera data eller interagera med webbsidor.

## Så här gör du:
För att parse:a HTML i Rust, använd biblioteket `scraper`. Först, lägg till beroendet i din `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Importera och använd sedan så här:

```rust
use scraper::{Html, Selector};

fn main() {
    let html = r#"
        <ul>
            <li>Rust</li>
            <li>HTML</li>
            <li>Parsing</li>
        </ul>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("li").unwrap();
    
    for element in document.select(&selector) {
        println!("{}", element.inner_html());
    }
}
```

Exekvera koden och du borde se:

```
Rust
HTML
Parsing
```

## Djupdykning
Parsing av HTML är inte nytt. I tidiga webbdagar hanterades det oftast server-sidan via CGI-skript. Nu har det blivit en del av många språk och ramverk, som Rust med `scraper` eller JavaScript med `Cheerio`.

Andra bibliotek i Rust som kan användas för att parse:a HTML inkluderar `html5ever` och `select.rs`. `scraper` förlitar sig faktiskt på `html5ever` för parsing, vilket är byggt av Servo-projektet och är extremt snabbt och robust.

Implementeringsdetaljer inkluderar att hantera olika typer av HTML-dokument, såsom slice eller `String`, och felhantering när HTML inte kan parse:as korrekt (t.ex. när HTML inte är välskriven eller saknar nödvändiga taggar).

## Se även
- `scraper` dokumentation: [docs.rs/scraper](https://docs.rs/scraper)
- `html5ever` GitHub sida: [github.com/servo/html5ever](https://github.com/servo/html5ever)
- Rust officiella webbplats och dokumentation: [rust-lang.org](https://www.rust-lang.org/)
