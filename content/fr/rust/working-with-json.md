---
title:                "Travailler avec json"
html_title:           "Rust: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi travailler avec JSON en Rust ? Eh bien, il s'agit d'un format de données universellement pris en charge, ce qui en fait un choix populaire pour les échanges de données entre différentes applications et systèmes. En utilisant Rust, vous bénéficierez également de sa sécurité et de sa performance, ce qui rend le traitement de fichiers JSON plus rapide et moins sujet aux erreurs.

## Comment Faire

Pour commencer à travailler avec JSON en Rust, vous avez besoin d'un crate (package) appelé `serde_json`. Vous pouvez l'ajouter à votre projet en ajoutant la dépendance suivante dans votre `Cargo.toml` :

```rust
[dependencies]
serde_json = "0.11.2"
```

Ensuite, vous pouvez importer le crate dans votre code :

```rust
extern crate serde_json;
use serde_json::json;
```

Maintenant, vous êtes prêt à travailler avec JSON ! Voici un exemple simple de création d'un objet JSON et de l'écriture dans un fichier :

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let name = "John";
    let age = 30;
    let hobbies = vec!["reading", "cooking", "hiking"];

    let person = json!({
        "name": name,
        "age": age,
        "hobbies": hobbies,
    });

    let mut file = File::create("person.json").expect("Unable to create file");
    file.write_all(person.to_string().as_bytes())
        .expect("Unable to write data to file");
}
```

Le résultat sera un fichier `person.json` contenant :

```json
{
    "name": "John",
    "age": 30,
    "hobbies": [
        "reading",
        "cooking",
        "hiking"
    ]
}
```

Vous pouvez également utiliser `serde_json` pour lire des fichiers JSON et les convertir en structures de données en Rust. Voici un exemple de lecture du fichier `person.json` créé précédemment :

```rust
let file = File::open("person.json").expect("Unable to open file");
let person: Person = serde_json::from_reader(file).expect("Unable to parse data from file");

println!("Name: {}", person.name);
println!("Age: {}", person.age);
println!("Hobbies: {:?}", person.hobbies);
```

## Plongée en Profondeur

`serde_json` offre également une grande flexibilité dans la manipulation de données JSON. Par exemple, vous pouvez utiliser des traits pour définir comment les champs d'une structure doivent être sérialisés ou désérialisés :

```rust
use serde_json::{Deserializer, ser::Serializer, value::RawValue};

struct Post {
    title: String,
    body: String,
    date: String,
}

impl Serialize for Post {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Post", 3)?;
        state.serialize_field("title", self.title)?;
        state.serialize_field("body", self.body)?;
        state.serialize_field("date", self.date)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Post {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field { Title, Body, Date }

        struct PostVisitor;

        impl<'de> Visitor<'de> for PostVisitor {
            type Value = Post;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Post")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: seq::SeqAccess<'de>,
            {
                Ok(Post {
                    title: seq.next_element()?
                        .ok_or_else(|| de::Error::invalid_length(0, &self))?,
                    body: seq.next_element()?
                        .ok_or_else(|| de::Error::invalid_length(1, &self))?,
                    date: seq.next_element()?
                        .ok_or_else(|| de::Error::invalid_length(2, &self))?,
                })
            }

            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut