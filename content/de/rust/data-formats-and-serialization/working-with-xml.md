---
title:                "Arbeiten mit XML"
aliases:
- /de/rust/working-with-xml.md
date:                  2024-01-26T04:35:21.198965-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
XML, die Abkürzung für eXtensible Markup Language, ist wie der geschwätzige Cousin von JSON. Mit XML setzen Sie sich auseinander, wenn es um Altlastensysteme, Unternehmenssoftware oder APIs geht, die auf den JSON-Zug verzichtet haben. Es ist wesentlich für den Datenaustausch, wo XML seine Stärke beweist.

## Wie geht das:
In Rust können Sie XML mit Crates wie `xml-rs` bearbeiten. Installieren Sie es, indem Sie `xml-rs = "0.8"` zu Ihrer `Cargo.toml` hinzufügen. So parsen Sie ein einfaches XML:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("Start: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Text: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Ende: {}", name);
            }
            Err(e) => {
                println!("Fehler: {}", e);
            }
            _ => {}
        }
    }
}
```

Ausgabe:
```
Start: book
Start: title
Text: Rust in Action
Ende: title
Start: author
Text: Tim McNamara
Ende: author
Start: year
Text: 2021
Ende: year
Ende: book
```
Dieser Code liest XML stream-weise, behandelt Start- und Endelemente sowie Textdaten und protokolliert jeden Schritt.

## Tiefer Eintauchen:
XML ist ein Senior in Technikjahren, entworfen für das Web in den späten 90ern. Sein Design fördert die Lesbarkeit (sowohl für Maschinen als auch für Menschen) und umfangreiche selbstbeschreibende Daten.

Alternativen? Klar, JSON ist die moderne Lösung für Web-APIs, leichter und weniger störend. In der Zwischenzeit hat YAML Fans für Konfigurationen gesammelt, mit seinem sauberen Layout. Aber XML wird so schnell nicht verschwinden – riesige Infrastrukturen sind auf seinem Rücken aufgebaut.

Unter der Haube setzt Rusts XML-Parsing auf Iterator-Muster, hält den Speicherverbrauch niedrig und die Leistung scharf. Sie finden Crates wie `serde-xml-rs` für ein mehr serde-ähnliches Erlebnis – ein Segen für diejenigen, die an JSON-Handling gewöhnt sind.

## Siehe auch:
Für mehr über Rust und XML: 
- `serde-xml-rs` für die serde-Kompatibilität in Rust: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Offizielle Rust-Dokumentation (weil es nie schadet, sich auf den neuesten Stand zu bringen): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
