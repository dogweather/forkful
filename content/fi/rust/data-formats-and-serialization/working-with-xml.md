---
title:                "XML:n käsittely"
aliases: - /fi/rust/working-with-xml.md
date:                  2024-01-26T04:35:29.618764-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML, lyhenne sanoista eXtensible Markup Language, on kuin JSONin sanavalmis serkku. Joudut painimaan XML:n kanssa, kun työskentelet perintöjärjestelmien, yritysohjelmistojen tai APIen parissa, jotka ohittivat JSON-buumin. Se on olennainen tiedonvaihdossa, jossa XML pitää pintansa.

## Kuinka:
Rustissa voit käsitellä XML:ää paketeilla, kuten `xml-rs`. Asenna lisäämällä `xml-rs = "0.8"` tiedostoosi `Cargo.toml`. Tässä on esimerkki yksinkertaisen XML:n jäsentämisestä:

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
                println!("Alku: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Teksti: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Loppu: {}", name);
            }
            Err(e) => {
                println!("Virhe: {}", e);
            }
            _ => {}
        }
    }
}
```

Tuloste:
```
Alku: book
Alku: title
Teksti: Rust in Action
Loppu: title
Alku: author
Teksti: Tim McNamara
Loppu: author
Alku: year
Teksti: 2021
Loppu: year
Loppu: book
```
Tämä koodi lukee XML:ää virtana, käsitellen alku- ja lopputageja sekä tekstidataa, kirjaten jokaisen vaiheen.

## Syväsukellus:
XML on teknologiavuosissa seniori, kehitetty webille 90-luvun lopulla. Sen suunnittelun tavoitteena on lukeevoisuus (sekä koneille että ihmisille) ja kattava itsekuvaileva data.

Vaihtoehtoja? Tietenkin, JSON on nykyaikainen valinta web-API:lle, kevyempi ja vähemmän meluisa. Samaan aikaan YAML on kerännyt faneja konfiguraatioille sen siistin asettelun ansiosta. Mutta XML ei ole häviämässä mihinkään pian - valtavat infrastruktuurit on rakennettu sen varaan.

Taustalla Rustin XML-jäsennys nojautuu iteraattorimalleihin, pitäen muistinkäytön alhaisena ja suorituskyvyn terävänä. Löydät paketteja kuten `serde-xml-rs` serde-tyylisempään kokemukseen - aarre niille, jotka ovat tottuneet JSONin käsittelyyn.

## Katso Myös:
Lisää tietoa Rustista ja XML:stä:
- `serde-xml-rs` Rustin serde-yhteensopivuudelle: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Virallinen Rust-dokumentaatio (koska kertaaminen ei koskaan ole pahitteeksi): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
