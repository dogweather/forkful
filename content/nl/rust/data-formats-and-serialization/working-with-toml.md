---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:52.617697-07:00
description: "TOML is een voor mensen leesbare taal voor data-serialisatie, vaak gebruikt\
  \ voor configuraties. Programmeurs gebruiken TOML vanwege de eenvoud en\u2026"
lastmod: '2024-03-13T22:44:50.617334-06:00'
model: gpt-4-0125-preview
summary: "TOML is een voor mensen leesbare taal voor data-serialisatie, vaak gebruikt\
  \ voor configuraties. Programmeurs gebruiken TOML vanwege de eenvoud en\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML is een voor mensen leesbare taal voor data-serialisatie, vaak gebruikt voor configuraties. Programmeurs gebruiken TOML vanwege de eenvoud en duidelijkheid, wat gemakkelijk vertaalt naar een hash map in Rust.

## Hoe te:
```Rust
// 1. Voeg de 'toml' crate toe aan je Cargo.toml
// [dependencies]
// toml = "0.5"

// 2. Deserializeer TOML naar een struct in Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("De server draait op {}:{}", host, port);
    // Uitvoer: De server draait op "localhost":8080
}
```

## Diepere Duik
TOML, dat staat voor Tom's Obvious, Minimal Language, is gecreëerd door Tom Preston-Werner in 2013. Het doel is om leesbaarder te zijn dan JSON of YAML voor config bestanden. TOML's ontwerp richt zich op ondubbelzinnige syntaxis, minimalisme, en eenvoudige mapping naar datatypes.

Alternatieven voor TOML omvatten JSON, YAML, en XML, maar TOML wint in scenario's waar menselijke leesbaarheid en het bewerken van bestanden door niet-programmeurs cruciaal is. Bij het werken met TOML in Rust biedt serde een sterke basis voor serialisatie en deserialisatie, door gebruik te maken van traits om TOML moeiteloos op Rust's structs te mappen.

Een uitdaging bij het werken met TOML is de striktheid op types en structuur. De programmeur moet een goed gestructureerd Rust typesysteem definiëren dat de schema van de TOML data weerspiegelt om TOML effectief in Rust te kunnen gebruiken.

## Zie Ook
- [TOML Documentatie](https://toml.io/en/)
- [serde_toml Crate](https://docs.rs/serde_toml/)
- [Rust Programmeertaal Boek](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub Repo](https://github.com/toml-lang/toml)
