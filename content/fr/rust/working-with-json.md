---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec JSON, c'est manipuler un format de données léger pour l'échange d'informations. Les programmeurs l'utilisent car c'est un standard web facile à comprendre pour les humains et à parser pour les machines.

## Comment faire :
```Rust
use serde::{Deserialize, Serialize};
use serde_json::Result;

// Une structure pour nos données, à sérialiser/désérialiser.
#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    is_programmer: bool,
}

fn main() -> Result<()> {
    // Créons une personne.
    let person = Person {
        name: "Alex".to_owned(),
        age: 30,
        is_programmer: true,
    };

    // Convertissons la personne en JSON.
    let serialized = serde_json::to_string(&person)?;
    println!("Modèle JSON: {}", serialized);

    // Récupérons la personne à partir de la chaîne JSON.
    let deserialized: Person = serde_json::from_str(&serialized)?;
    println!("Nom: {}", deserialized.name);
    Ok(())
}
```
Sortie échantillon :
```
Modèle JSON: {"name":"Alex","age":30,"is_programmer":true}
Nom: Alex
```

## Fouille détaillée
Historiquement, JSON (JavaScript Object Notation) provient de JavaScript, mais s'est émancipé comme format indépendant devenu un standard ECMA. Rust, avec des crates comme `serde_json`, offre une expérience fluide en sérialisant/désérialisant des structures Rust en JSON et vice-versa. En alternatives, on trouve XML, moins ergonomique, ou des formats binaires comme Protobuf, efficaces mais moins accessibles.

## Voir également
- La documentation officielle de `serde_json`: [serde.rs](https://serde.rs/)
- Le RFC pour JSON en tant que standard: [RFC 7159](https://tools.ietf.org/html/rfc7159)
- Une comparaison des formats d'échange de données: [Rust Data Serialization Comparison](https://github.com/serde-rs/serde#comparison-with-other-data-formats)
