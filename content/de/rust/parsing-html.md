---
title:                "HTML analysieren."
html_title:           "Rust: HTML analysieren."
simple_title:         "HTML analysieren."
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Parsing HTML ist der Prozess des Extrahierens von Daten aus HTML-Quellcode. Es ist eine wichtige Aufgabe für Programmierer, da es ihnen ermöglicht, Daten von Webseiten zu sammeln und zu analysieren.

## So geht's:
Um HTML in Rust zu parsen, kann das Bibliothekspaket "html5ever" verwendet werden. Der folgende Code zeigt ein einfaches Beispiel, wie man eine HTML-Seite lädt, das DOM-Dokument erfasst und die Titelüberschrift ausgibt:

```Rust
use html5ever::{parse_document, tendril::TendrilSink};

fn main() {
    let input = r#"
        <html>
            <head>
                <title>Meine Webseite</title>
            </head>
            <body>
                <h1>Willkommen!</h1>
            </body>
        </html>"#;
    let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut input.as_bytes())
        .unwrap();
    let doc = dom.document;
    let h1 = doc
        .children[0]
        .children[1]
        .children[0]
        .children[1]
        .children[0]
        .as_text()
        .unwrap();
    println!("Titel: {}", h1);
}

// Ausgabe:
// Titel: Willkommen!
```

## Tiefere Einblicke:
Parsing HTML ist seit den Anfängen des World Wide Web eine wichtige Aufgabe. Früher wurde dies mit regulären Ausdrücken oder anderen Sprachen wie Perl oder Python durchgeführt. Heutzutage gibt es jedoch spezielle Bibliotheken und Werkzeuge, die dies effizienter und zuverlässiger erledigen können.

Alternativ zu "html5ever" gibt es auch andere Bibliotheken wie "kuchiki", "scraper" oder "select". Diese unterscheiden sich in ihrer Syntax und Funktionalität, können aber ebenfalls zum Parsen von HTML verwendet werden.

Die Implementierung von "html5ever" basiert auf dem HTML-Standardspezifikator. Es ist eine Parser-Bibliothek, die das Parsen von HTML gemäß den offiziellen Spezifikationen durchführt.

## Siehe auch:
- Dokumentation zur Bibliothek "html5ever": https://docs.rs/html5ever/
- Weitere Bibliotheken zum Parsing von HTML in Rust: https://github.com/brson/parse-html-rs
- Offizieller HTML-Standardspezifikator: https://html.spec.whatwg.org/