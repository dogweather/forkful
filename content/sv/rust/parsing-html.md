---
title:                "Analysera html"
html_title:           "Rust: Analysera html"
simple_title:         "Analysera html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att analysera HTML-filer är en viktig del av många webbutvecklingsprojekt. Det kan hjälpa till att extrahera information från hemsidor, utveckla webbautomatiseringstester och mycket mer.

## Hur man gör det

För att analysera HTML-filer i Rust finns det flera tredjepartsbibliotek tillgängliga, men vi kommer att fokusera på biblioteket "scraper". För att använda detta bibliotek behöver du ha Rust installerat på din dator.

### Installation

Först och främst måste du installera "scraper" biblioteket. Detta görs genom att lägga till det under `[dependencies]` sektionen i din `Cargo.toml` fil.

```Rust
[dependencies]
scraper = "0.13.0"
```

Du kan sedan installera biblioteket genom att köra följande kommando i din terminal:

```bash
cargo build
```

### Exempelkod

När biblioteket har installerats kan du börja använda det för att analysera HTML-filer. Se nedan för ett exempel på hur du kan extrahera titeln på en hemsida med hjälp av "scraper":

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = r#"
        <html>
          <head>
            <title>Min hemsida</title>
          </head>
          <body>
            <h1>Här är min hemsida</h1>
            <p>Välkommen!</p>
          </body>
        </html>
    "#;

    let document = Html::parse_document(html);
    let title_selector = Selector::parse("title").unwrap();

    for title in document.select(&title_selector) {
        println!("{}", title.text().collect::<Vec<_>>().join(""));
    }
}
```

Detta kommer att skriva ut "Min hemsida" i konsolen. Du kan anpassa exemplet för att extrahera andra element på en hemsida, som länkar eller bilder.

## Djupdykning

För mer avancerade användningsområden, kan du också utforska andra bibliotek för att analysera HTML i Rust. Några populära alternativ är "html5ever" och "kuchiki". Dessa bibliotek har fler funktioner och kan vara mer lämpliga för professionella projekt. Du kanske också vill titta på hur man använder CSS-väljare för att lättare hitta specifika element på en sida.

## Se även

- [scraper dokumentation](https://docs.rs/scraper/0.13.0/scraper/)
- [html5ever dokumentation](https://docs.rs/html5ever/0.24.0/html5ever/)
- [kuchiki dokumentation](https://docs.rs/kuchiki/0.9.0/kuchiki/)