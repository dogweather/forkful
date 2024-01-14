---
title:                "Rust: Att analysera html"
simple_title:         "Att analysera html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Om du någonsin har jobbat med webbutveckling har du förmodligen stött på HTML-kod. Att hantera och tolka denna typ av kod kan vara en utmaning, men med hjälp av Rust kan du underlätta processen och göra det på ett effektivt sätt.

## Så här gör du
För att parsning av HTML med Rust behöver du först importera biblioteket scraper. Detta bibliotek gör det möjligt att ladda ned och granska webbsidor från internet, inklusive dess HTML-kod.

```Rust
extern crate scraper;

use scraper::{Html, Selector}; // Importera scraper biblioteket

fn main() {
    // Skapa en variabel med HTML-koden du vill parsa
    let html_doc = r#"
        <html>
          <head>
            <title>Hello, world!</title>
          </head>
          <body>
            <h1>Hej världen!</h1>
          </body>
        </html>
    "#;

    // Konvertera HTML-kod till Rusts selektortyp
    let document = Html::parse_document(html_doc);

    // Använd selector-funktion för att välja den del av koden du vill extrahera
    let h1_selector = Selector::parse("h1").unwrap();

    // Iterera över alla valda element och skriv ut dess text
    for h1 in document.select(&h1_selector) {
        println!("Text: {}", h1.text());
    }
}
```

Detta kodexempel ska resultera i utskriften "Text: Hej världen!", vilket visar hur man enkelt kan extrahera innehåll från HTML-kod.

## Djupdykning
När du är bekväm med grundläggande kodexempel som detta kan du börja utforska fler möjligheter för att hantera och parsning av HTML genom att använda scraper-bibliotekets andra funktioner. Det finns också andra bibliotek tillgängliga som kan hjälpa till med HTML-parsning, såsom kuchiki och select. Dessa bibliotek kan ha olika funktioner och fördelar, så det är en god idé att undersöka och jämföra dem för att hitta den som passar bäst för dina behov.

## Se även
- [Scraper dokumentation] (https://docs.rs/scraper/0.9.0/scraper/)
- [Kuchiki dokumentation] (https://docs.rs/kuchiki/0.7.0/kuchiki/)
- [Select dokumentation] (https://docs.rs/select/0.3.0/select/)