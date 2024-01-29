---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:43.484470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
JSON (JavaScript Object Notation) is een tekstformaat voor gegevensuitwisseling. Programmeurs gebruiken het vanwege de eenvoud en interoperabiliteit tussen talen, waardoor het delen van gegevens tussen diensten en applicaties naadloos verloopt.

## Hoe te:

In Rust zijn de `serde` en `serde_json` crates de go-to bibliotheken voor werk met JSON. Hier is hoe je ze gebruikt:

Voeg eerst afhankelijkheden toe in `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Importeer vervolgens de crates en definieer een struct om je gegevens te vertegenwoordigen:

```rust
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

#[derive(Serialize, Deserialize, Debug)]
struct Gebruiker {
    id: u64,
    naam: String,
    email: String,
}

fn main() {
    // Serialiseren
    let gebruiker = Gebruiker {
        id: 1,
        naam: "Jane Doe".to_string(),
        email: "jane.doe@voorbeeld.com".to_string(),
    };
    let j = serde_json::to_string(&gebruiker).unwrap();
    println!("{}", j); // {"id":1,"naam":"Jane Doe","email":"jane.doe@voorbeeld.com"}

    // Deserialiseren
    let e: Gebruiker = serde_json::from_str(&j).unwrap();
    println!("{:?}", e);  // Gebruiker { id: 1, naam: "Jane Doe", email: "jane.doe@voorbeeld.com" }
}
```

## Diepgang:

Het `serde` bibliotheeksysteem is sinds de release in 2015 de de facto oplossing van Rust voor serialisatie. Het ondersteunt veel formaten naast JSON. Afwisselend kun je `json-rust` of `simd-json` tegenkomen, die verschillende prestatieafwegingen bieden. Een cruciaal implementatiedetail om te begrijpen is dat `serde` deserialisatie vereist dat gegevensstructuren bekend zijn op compileertijd, wat niet het geval is bij meer dynamische talen zoals JavaScript.

## Zie Ook:

- Serde's officiële documentatie biedt een uitgebreide handleiding: [Serde Documentatie](https://serde.rs)
- De onderliggende details van de `serde_json` crate: [serde_json Crate](https://docs.rs/serde_json)
- Meer over JSON zelf: [Introductie van JSON](https://www.json.org/json-en.html)
- Voor asynchroon programmeren met JSON worden `tokio` en `async-std` vaak gebruikt in combinatie met `serde_json`.
