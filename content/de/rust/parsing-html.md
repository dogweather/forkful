---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-html.md"
---

{{< edit_this_page >}}

# Rust und HTML Parsing: Ein praktischer Ansatz

## Was und Warum?

HTML-Parsing bezeichnet die Zerlegung von HTML in seine Bestandteile, um sie für Programmierungszwecke nutzbar zu machen. Programmierer machen dies, um Inhalte aus Webseiten zu extrahieren oder um Webseiten zu testen und zu debuggen.

## So geht's:

Wir werden mit dem "scraper" Paket arbeiten. Beginnen wir mit der Einbindung des Pakets in unsere cargo.toml Datei.

```Rust
[dependencies]
scraper = "0.12.0"
```

Jetzt erstellen wir einen einfachen Parser:

```Rust
extern crate scraper;
use scraper::{Html, Selector};

fn main() {
    let html = r#"<p class='red'>Hallo, Welt!</p>"#;
    let document = Html::parse_document(&html);
    let selector = Selector::parse(".red").unwrap();

    for element in document.select(&selector) {
        let text = element.text().collect::<Vec<_>>();
        println!("{}", text[0]);
    }
}
```

Wenn Sie das ausführen, erhalten Sie den Ausdruck "Hallo, Welt!".

## Vertiefung

Historisch gesehen ist HTML-Parsing direkt mit dem Aufkommen des Webs und der Notwendigkeit verbunden, Websiten zugänglich und nutzbar zu machen. Es gibt viele Alternativen zum HTML-Parsing in Rust, wie z.B. html5ever und kuchiki, die allerdings unterschiedliche Funktionalitäten und Spezifitäten bieten.

Die Implementierung von HTML-Parsing variiert stark je nach Sprache und den spezifischen Anforderungen des Projekts. In Rust konzentrieren wir uns häufig auf Effizienz und Genauigkeit, einschließlich der Fähigkeit, Fehler zu erkennen und darauf zu reagieren.

## Siehe auch

Weitere Informationen, Beispiele und Ressourcen zum HTML-Parsing in Rust finden Sie unter folgenden Links:

- Rust-Dokumentation: https://doc.rust-lang.org/book/
- Scraper Crate Dokumentation: https://docs.rs/scraper/0.12.0/scraper/
- Rust Cookbook Eintrag zum HTML-Parsing: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
- html5ever Repo: https://github.com/servo/html5ever
- Kuchiki Repo: https://github.com/kuchiki-rs/kuchiki