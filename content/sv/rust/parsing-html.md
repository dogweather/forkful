---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML-parsning handlar om att omvandla HTML-kod till en strukturerad representation, ofta ett syntaxträd. Programmerare gör detta för att hämta, ändra, eller extrahera information från webbsidor.

## Hur man gör:
Använd `html5ever`-biblioteket för att parsa HTML-kod. Installera det genom att lägga till detta till din Cargo.toml fil:

```Rust
[dependencies]
html5ever = "0.24.1"
```

Här är ett enkelt exempel:
```Rust
extern crate html5ever;
use html5ever::driver::ParseOpts;
use html5ever::rcdom::RcDom;
use html5ever::tendril::TendrilSink;

fn main() {
    let html = "<p>Hello World</p>";
    let dom = parse_html(html);

    // ... hantera dom-objektet
}

fn parse_html(html: &str) -> RcDom {
    let opts = ParseOpts::default();
    let dom = html5ever::parse_document(RcDom::default(), opts)
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .unwrap();

    dom
}
```

## Fördjupning
Historiskt sett var HTML-parsning ovanligt inom programmering, men eftersom webben har växt har behovet ökat avsevärt. Det finns alternativ till `html5ever`, som `lxml` i Python och `jsoup` i Java.

HTML5ever, som vi använde här, är måhända det bästa valet i Rust. Det är en del av Servo-projektet och är utformat för att hantera den verkliga, icke-perfekta HTML som man ofta hittar i det vilda på webben.

## Se även
- Rust Programmering bok: [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
- HTML5ever dokumentation: [https://docs.rs/html5ever/0.24.1/html5ever/](https://docs.rs/html5ever/0.24.1/html5ever/)
- Servo: [https://github.com/servo/servo](https://github.com/servo/servo)