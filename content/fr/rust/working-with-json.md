---
title:                "Rust: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON est un format de données populaire en informatique, utilisé pour échanger des informations entre différents systèmes. Il est donc important pour tout programmeur de savoir comment travailler avec JSON, en utilisant un langage de programmation moderne comme Rust.

## Comment faire

Pour travailler avec JSON en Rust, vous aurez besoin d'importer la bibliothèque `serde_json`. Voici un exemple de code pour sérialiser un objet JSON et l'imprimer en tant que chaîne de caractères:

```
Rust
use serde_json::{Result, Value};

fn main() -> Result<()> {
    // Création d'un objet JSON
    let data = r#"{
        "nom": "Marie",
        "âge": 30,
        "études": ["Informatique", "Mathématiques"],
        "mariée": false
    }"#;

    // Sérialisation de l'objet en chaîne de caractères
    let v: Value = serde_json::from_str(data)?;

    // Impression de la chaîne de caractères
    println!("{}", serde_json::to_string_pretty(&v)?);

    Ok(())
}
```

Lorsque vous exécutez ce code, vous obtiendrez l'objet JSON imprimé en tant que chaîne de caractères avec un formatage facile à lire:

```
{
    "nom": "Marie",
    "âge": 30,
    "études": [
        "Informatique",
        "Mathématiques"
    ],
    "mariée": false
}
```

Mais comment pouvons-nous travailler avec des données JSON plus complexes, comme des tableaux d'objets? Voici un exemple pour vous montrer comment accéder à ces données en utilisant des méthodes de l'objet `Value`:

```
Rust
use serde_json::{from_str, Value};

fn main() {
    // Une chaîne de caractères JSON avec un tableau d'objets
    let data = r#"
        [
            {
                "nom": "Paul",
                "âge": 25
            },
            {
                "nom": "Marie",
                "âge": 30
            },
            {
                "nom": "Luc",
                "âge": 35
            }
        ]
    "#;

    // Utilisation de la méthode `from_str` pour sérialiser les données en `Value`
    let v: Value = from_str(data).unwrap();

    // Accès aux données en utilisant des index
    println!("Le nom du premier élément est: {}", v[0]["nom"]);
    println!("L'âge du troisième élément est: {}", v[2]["âge"]);
}
```

Lorsque vous exécutez ce code, vous verrez que nous pouvons facilement accéder aux données JSON en utilisant des index comme si nous travaillions avec un tableau.

## Deep Dive

Maintenant que vous avez une idée de base de la façon de travailler avec JSON en Rust, vous pouvez approfondir encore plus en apprenant sur `serde`, la bibliothèque sous-jacente utilisée par `serde_json`. `serde` est une bibliothèque puissante pour la sérialisation et la désérialisation de données, et elle est utilisée dans de nombreux projets Rust.

De plus, vous pouvez également expérimenter avec les différentes méthodes et outils offerts par la bibliothèque `serde_json` pour une utilisation plus avancée, comme la validation de données JSON et la manipulation avancée de données.

## Voir aussi

Pour en savoir plus sur la manipulation de données JSON en Rust, voici quelques liens utiles:

- [Documentation de `serde_json`](https://docs.serde.rs/serde_json/) - la documentation officielle de la bibliothèque `serde_json`.
- [Guide JSON avec `serde`](https://github.com/serde-rs/json/blob/master/README.md) - un guide complet de utilisation de `serde` pour travailler avec JSON en Rust.
- [Projet GitHub de `serde`](https://github.com/serde-rs/serde) - le code source et la documentation officiels de la bibliothèque `serde`.
- [Projet GitHub de `serde_json`](https://github.com/serde-rs/json) - le code source et la documentation officiels de la bibliothèque `serde_json`.

Maintenant que vous avez les bases pour travailler avec JSON en Rust, vous pouvez commencer à créer des applications puissantes et fiables