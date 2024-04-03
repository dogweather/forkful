---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:02.260111-07:00
description: "Het parsen van HTML is het proces waarbij een string van HTML wordt\
  \ ontleed en omgezet in een gegevensstructuur die je programma kan begrijpen en\u2026"
lastmod: '2024-03-13T22:44:50.591948-06:00'
model: gpt-4-0125-preview
summary: Het parsen van HTML is het proces waarbij een string van HTML wordt ontleed
  en omgezet in een gegevensstructuur die je programma kan begrijpen en manipuleren.
title: HTML Parsen
weight: 43
---

## Hoe doe je dat:
Om HTML in Rust te parsen, wil je waarschijnlijk een crate zoals `scraper` of `select` gebruiken. Hier is een snel voorbeeld met `scraper`:

```Rust
use scraper::{Html, Selector};

fn main() {
    // De HTML invoer als een string
    let html = r#"
        <html>
            <body>
                <p>Hallo, wereld!</p>
            </body>
        </html>
    "#;

    // Parse de HTML string
    let document = Html::parse_document(html);
    
    // Creëer een selector om alle <p> tags te vinden
    let selector = Selector::parse("p").unwrap();

    // Itereer over elementen die overeenkomen met de selector
    voor element in document.select(&selector) {
        // Print de tekst binnen elke <p> tag
        println!("{}", element.text().collect::<Vec<_>>().concat());
    }
}
```

Output:
```
Hallo, wereld!
```

## Diepere duik
Vroeger was het parsen van HTML een rommelige onderneming. Bibliotheken verschilden, standaarden waren voortdurend in beweging en de aanpak varieerde per taal. Vandaag de dag biedt Rust's ecosysteem robuuste crates voor het parsen, zoals `scraper` die steunt op de `html5ever` en `selectors` bibliotheken. `html5ever` is bijzonder interessant; het is gebaseerd op het HTML-parseringsalgoritme gespecificeerd door de WHATWG, waardoor het gelijk staat aan hoe moderne browsers HTML parsen.

Alternatieven voor `scraper` zijn onder andere `select`, dat vergelijkbare functionaliteit, maar andere ergonomie biedt. Laagniveau-parsing is mogelijk met `html5ever` zelf als je meer controle nodig hebt.

Vaak maakt het parsen van HTML deel uit van webscraping, waarbij je gegevens van websites extraheert. Het is belangrijk (en ethisch) om de `robots.txt` en de servicevoorwaarden van een site te respecteren bij het scrapen.

Wat implementatie betreft, onthoud altijd dat parsen slechts het beginpunt is. Sanitisatie en validatie zijn essentieel om beveiligingsproblemen zoals XSS (Cross-Site Scripting) aanvallen te voorkomen, vooral als je van plan bent om geparseerde gegevens weer te geven of op te slaan.

## Zie ook
- De `scraper` crate: https://crates.io/crates/scraper
- De `select` crate: https://crates.io/crates/select
- De `html5ever` GitHub repo: https://github.com/servo/html5ever
- De sectie "Web scraping" van het Rust Cookbook: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
- WHATWG HTML parsing specificatie: https://html.spec.whatwg.org/multipage/parsing.html
- De handleiding van Rust over foutafhandeling: https://doc.rust-lang.org/book/ch09-00-error-handling.html (om te gaan met mogelijke `unwrap` panics)
