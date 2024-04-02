---
date: 2024-01-26 04:35:54.886971-07:00
description: "XML, abbreviazione di eXtensible Markup Language, \xE8 come il cugino\
  \ prolisso di JSON. Ti troverai a lottare con l'XML quando avrai a che fare con\
  \ sistemi\u2026"
lastmod: '2024-03-13T22:44:43.242825-06:00'
model: gpt-4-0125-preview
summary: "XML, abbreviazione di eXtensible Markup Language, \xE8 come il cugino prolisso\
  \ di JSON. Ti troverai a lottare con l'XML quando avrai a che fare con sistemi\u2026"
title: Lavorare con XML
weight: 40
---

## Cosa & Perché?
XML, abbreviazione di eXtensible Markup Language, è come il cugino prolisso di JSON. Ti troverai a lottare con l'XML quando avrai a che fare con sistemi legacy, software enterprise o API che hanno saltato il carrozzone JSON. È essenziale per lo scambio di dati dove l'XML mantiene la sua posizione.

## Come fare:
In Rust, puoi gestire XML con crate come `xml-rs`. Installa aggiungendo `xml-rs = "0.8"` al tuo `Cargo.toml`. Ecco come analizzare un semplice XML:

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
                println!("Inizio: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Testo: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Fine: {}", name);
            }
            Err(e) => {
                println!("Errore: {}", e);
            }
            _ => {}
        }
    }
}
```

Output:
```
Inizio: book
Inizio: title
Testo: Rust in Action
Fine: title
Inizio: author
Testo: Tim McNamara
Fine: author
Inizio: year
Testo: 2021
Fine: year
Fine: book
```
Questo codice legge lo stream di XML, gestendo elementi di inizio e fine più i dati di testo, registrando ogni passaggio.

## Approfondimento:
L'XML è un veterano negli anni tecnologici, creato per il web alla fine degli anni '90. Il suo design promuove la leggibilità (sia per le macchine che per gli umani) e dati auto-descrittivi estensivi.

Alternative? Certo, JSON è il moderno punto di riferimento per le API web, più leggero e meno rumoroso. Nel frattempo, YAML ha raccolto fan per le configurazioni, con il suo layout pulito. Ma l'XML non andrà da nessuna parte a breve—vaste infrastrutture sono costruite sulla sua base.

Sotto il cofano, l'analisi dell'XML in Rust si basa su schemi iteratore, mantenendo l'uso della memoria basso e le prestazioni elevate. Troverai crate come `serde-xml-rs` per un'esperienza più in linea con serde—una manna per chi è abituato alla gestione JSON.

## Vedi Anche:
Per saperne di più su Rust e XML: 
- `serde-xml-rs` per la compatibilità con serde di Rust: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Documentazione ufficiale di Rust (perché non fa mai male fare un ripasso): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
