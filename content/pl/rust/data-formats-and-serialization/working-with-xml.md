---
date: 2024-01-26 04:35:52.849089-07:00
description: "Jak to zrobi\u0107: W Rust, mo\u017Cna obs\u0142u\u017Cy\u0107 XML za\
  \ pomoc\u0105 crate'\xF3w takich jak `xml-rs`. Zainstaluj, dodaj\u0105c `xml-rs\
  \ = \"0.8\"` do twojego `Cargo.toml`. Oto jak\u2026"
lastmod: '2024-03-13T22:44:35.210879-06:00'
model: gpt-4-0125-preview
summary: "W Rust, mo\u017Cna obs\u0142u\u017Cy\u0107 XML za pomoc\u0105 crate'\xF3\
  w takich jak `xml-rs`."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
W Rust, można obsłużyć XML za pomocą crate'ów takich jak `xml-rs`. Zainstaluj, dodając `xml-rs = "0.8"` do twojego `Cargo.toml`. Oto jak przeanalizować prosty XML:

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
                println!("Początek: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Tekst: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Koniec: {}", name);
            }
            Err(e) => {
                println!("Błąd: {}", e);
            }
            _ => {}
        }
    }
}
```

Wyjście:
```
Początek: book
Początek: title
Tekst: Rust in Action
Koniec: title
Początek: author
Tekst: Tim McNamara
Koniec: author
Początek: year
Tekst: 2021
Koniec: year
Koniec: book
```
Ten kod odczytuje strumień XML, obsługując elementy początkowe i końcowe oraz dane tekstowe, rejestrując każdy krok.

## Szczegółowa analiza:
XML to senior w świecie technologii, stworzony dla sieci pod koniec lat 90. Jego design promuje czytelność (zarówno dla maszyn, jak i ludzi) i obszerne dane samoopisujące.

Alternatywy? Jasne, JSON jest współczesnym wyborem dla API sieciowych, lżejszym i mniej hałaśliwym. Tymczasem YAML zdobył fanów dla konfiguracji, dzięki swojemu czystemu układowi. Ale XML nie zniknie tak szybko – ogromne infrastruktury są zbudowane na jego podstawie.

Pod kapotą, parsowanie XML w Rust opiera się na wzorcach iteratorów, utrzymując niskie zużycie pamięci i wysoką wydajność. Znajdziesz crate'y takie jak `serde-xml-rs` dla bardziej serde-podobnego doświadczenia – bonus dla tych, którzy są przyzwyczajeni do obsługi JSON-a.

## Zobacz także:
Więcej na temat Rust i XML: 
- `serde-xml-rs` dla kompatybilności Rust z serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Oficjalna dokumentacja Rust (bo nigdy nie zaszkodzi się podszkolić): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
