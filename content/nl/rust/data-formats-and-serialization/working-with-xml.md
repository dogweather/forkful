---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:56.919248-07:00
description: "XML, de afkorting voor eXtensible Markup Language, is zoals de breedsprakige\
  \ neef van JSON. Je zult met XML worstelen wanneer je te maken hebt met legacy\u2026"
lastmod: '2024-03-13T22:44:50.618277-06:00'
model: gpt-4-0125-preview
summary: "XML, de afkorting voor eXtensible Markup Language, is zoals de breedsprakige\
  \ neef van JSON. Je zult met XML worstelen wanneer je te maken hebt met legacy\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?
XML, de afkorting voor eXtensible Markup Language, is zoals de breedsprakige neef van JSON. Je zult met XML worstelen wanneer je te maken hebt met legacy systemen, bedrijfssoftware, of API's die de overstap naar JSON niet hebben gemaakt. XML is essentieel voor gegevensuitwisseling waar het zijn grond behoudt.

## Hoe te:
In Rust kun je met XML omgaan met crates zoals `xml-rs`. Installeer door `xml-rs = "0.8"` toe te voegen aan je `Cargo.toml`. Hier is hoe je een eenvoudige XML kunt parsen:

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
                println!("Tekst: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Einde: {}", name);
            }
            Err(e) => {
                println!("Fout: {}", e);
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
Tekst: Rust in Action
Einde: title
Start: author
Tekst: Tim McNamara
Einde: author
Start: year
Tekst: 2021
Einde: year
Einde: book
```
Deze code leest XML stream-gewijs, en behandelt start- en eind-elementen plus tekstgegevens, en logt elke stap.

## Diepe Duik:
XML is een senior in tech jaren, ontworpen voor het web in de late jaren '90. Zijn ontwerp bevordert leesbaarheid (voor zowel machines als mensen) en uitgebreide zelfbeschrijvende gegevens.

Alternatieven? Natuurlijk, JSON is de moderne go-to voor web API's, lichter en minder rommelig. Ondertussen heeft YAML fans opgepikt voor configuraties, met zijn schone lay-out. Maar XML gaat voorlopig nergens heen—enorme infrastructuren zijn gebouwd op zijn rug.

Onder de motorkap leunt Rust's XML-parser op iteratorpatronen, houdt het geheugengebruik laag en de prestaties scherp. Je vindt crates zoals `serde-xml-rs` voor een meer serde-achtige ervaring—een zegen voor degenen die gewend zijn aan de omgang met JSON.

## Zie Ook:
Voor meer over Rust en XML:
- `serde-xml-rs` voor Rust's serde compatibiliteit: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Officiële Rust documentatie (want het kan nooit kwaad om op te frissen): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
