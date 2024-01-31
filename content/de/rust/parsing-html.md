---
title:                "HTML parsen"
date:                  2024-01-20T15:33:48.524713-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parser verwandeln HTML-Code in eine Struktur, die von Programmen verarbeitet werden kann. Das ist nötig, wenn man Daten aus Webseiten extrahieren oder die Struktur von Webseiten programmatisch verstehen möchte.

## So geht's:
```Rust
extern crate reqwest;
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    // HTML von einer Webseite holen
    let res = reqwest::blocking::get("https://www.rust-lang.org").unwrap();
    assert!(res.status().is_success());
    let body = res.text().unwrap();
    
    // HTML-Struktur parsen
    let document = Html::parse_document(&body);
    
    // Ein Selector, um z.B. nach h1-Tags zu suchen
    let selector = Selector::parse("h1").unwrap();
    
    // Durch die h1-Tags iterieren und den Textinhalt ausgeben
    for element in document.select(&selector) {
        println!("Gefundener Text: {}", element.text().collect::<Vec<_>>().join(""));
    }
}
```
Ausgabe könnte sein:
```
Gefundener Text: Empowering everyone to build reliable and efficient software.
```

## Tiefergehende Einblicke
Das Parsen von HTML ist kein neuer Trick. Seit dem Aufkommen von HTML haben Entwickler Bibliotheken und Tools entwickelt, um diese Aufgabe zu bewältigen. Historisch gesehen haben Programmiersprachen wie Perl und PHP hier Vorreiterarbeit geleistet. In Rust gibt es mehrere Bibliotheken, zum Beispiel `scraper` oder `html5ever`, die auf unterschiedliche Bedürfnisse zugeschnitten sind. `scraper` nutzt Selektoren ähnlich wie in JavaScript, um durch das HTML-Dokument zu navigieren, während `html5ever` einen Parser bietet, der den HTML5-Standard genau implementiert. Alternativen, wie Regex zum Parsen von HTML, werden oft nicht empfohlen, da HTML zu komplex für einfache Regex-Muster ist.

## Siehe auch
- Rust `scraper` Dokumentation: https://docs.rs/scraper/latest/scraper/
- `html5ever` GitHub Repository: https://github.com/servo/html5ever
- Zum weiterlesen: "Why regex is usually not the best choice for HTML parsing": https://stackoverflow.com/a/1732454
