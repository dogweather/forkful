---
date: 2024-01-26 04:35:50.743722-07:00
description: "Hur man g\xF6r: I Rust kan du hantera XML med crates som `xml-rs`. Installera\
  \ genom att l\xE4gga till `xml-rs = \"0.8\"` i din `Cargo.toml`. S\xE5 h\xE4r tolkar\
  \ du\u2026"
lastmod: '2024-03-13T22:44:37.723730-06:00'
model: gpt-4-0125-preview
summary: I Rust kan du hantera XML med crates som `xml-rs`.
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
I Rust kan du hantera XML med crates som `xml-rs`. Installera genom att lägga till `xml-rs = "0.8"` i din `Cargo.toml`. Så här tolkar du enkel XML:

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
    för e i parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("Start: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Text: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Slut: {}", name);
            }
            Err(e) => {
                println!("Fel: {}", e);
            }
            _ => {}
        }
    }
}
```

Output:
```
Start: book
Start: title
Text: Rust in Action
Slut: title
Start: author
Text: Tim McNamara
Slut: author
Start: year
Text: 2021
Slut: year
Slut: book
```
Denna kod ström-läser XML, hanterar start- och slutlement samt textdata, och loggar varje steg.

## Fördjupning:
XML är senior inom teknikår, skapad för webben i slutet av 90-talet. Dess design främjar läsbarhet (för både maskiner och människor) och omfattande självbeskrivande data.

Alternativ? Säkert, JSON är det moderna valet för webb-API:er, lättare och mindre bullrigt. Samtidigt har YAML fått fans för konfigurationer, med sin rena layout. Men XML kommer inte försvinna snart – omfattande infrastrukturer är byggda på dess rygg.

Under huven lutar sig Rusts XML-tolkning på iterator-mönster, håller minnesanvändningen låg och prestandan vass. Du kommer att hitta crates som `serde-xml-rs` för en mer serde-liknande upplevelse – en fördel för de som är vana vid JSON-hantering.

## Se även:
För mer om Rust och XML: 
- `serde-xml-rs` för Rusts serde-kompatibilitet: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Officiell Rust-dokumentation (för det skadar aldrig att fräscha upp kunskaperna): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
