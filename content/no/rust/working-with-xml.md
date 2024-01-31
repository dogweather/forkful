---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:36:00.017800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
XML, forkortet for eXtensible Markup Language, er som en ordrik fetter av JSON. Du vil støte på XML når du håndterer eldre systemer, bedriftsprogramvare, eller APIer som hoppet over JSON-toget. Det er essensielt for datautveksling der XML holder sin posisjon.

## Hvordan:
I Rust kan du håndtere XML med crates som `xml-rs`. Installer ved å legge til `xml-rs = "0.8"` i `Cargo.toml`. Her er hvordan du parser en enkel XML:

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
                println!("Slutt: {}", name);
            }
            Err(e) => {
                println!("Feil: {}", e);
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
Slutt: title
Start: author
Tekst: Tim McNamara
Slutt: author
Start: year
Tekst: 2021
Slutt: year
Slutt: book
```
Denne koden stream-leser XML, håndterer start- og sluttelementer pluss tekstdata, og logger hvert steg.

## Dypdykk:
XML er en senior i teknologiårene, laget for nettet på slutten av 90-tallet. Dets design fremmer lesbarhet (for både maskiner og mennesker) og omfattende selvbeskrivende data.

Alternativer? Sikkert, JSON er det moderne gå-til-valget for web-APIer, lettere og mindre støyende. I mellomtiden har YAML fått fans for konfigurasjoner, med sitt rene oppsett. Men XML går ikke noen vei med det første - enorme infrastrukturer er bygget på ryggen av det.

Under panseret, lener Rusts XML-parsing seg på iterator-mønstre, holder minnebruk lavt og ytelse skarp. Du vil finne crates som `serde-xml-rs` for en mer serde-lignende opplevelse - en gave for de som er vant til JSON-håndtering.

## Se også:
For mer om Rust og XML: 
- `serde-xml-rs` for Rusts serde-kompatibilitet: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Offisiell Rust-dokumentasjon (fordi det aldri skader å friske opp): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
