---
title:                "Travailler avec YAML"
aliases:
- /fr/rust/working-with-yaml/
date:                  2024-02-03T19:26:43.796251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

En programmation Rust, travailler avec YAML (YAML Ain't Markup Language) consiste à analyser et à générer des données au format YAML, un standard de sérialisation de données convivial pour l'humain. Les programmeurs intègrent la gestion de YAML dans Rust pour configurer des applications, gérer des paramètres ou traiter des structures de données complexes dans un format clair et lisible, tirant parti de sa simplicité par rapport à JSON ou XML pour les fichiers de configuration et l'échange de données.

## Comment faire :

Rust ne prend pas en charge YAML dans sa bibliothèque standard, donc nous utilisons couramment des crates tierces comme `serde` (pour la sérialisation et la désérialisation des données) en combinaison avec `serde_yaml`.

D'abord, ajoutez les dépendances à votre fichier `Cargo.toml` :

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Maintenant, voyons comment désérialiser une chaîne YAML en une structure Rust et sérialiser une structure Rust en retour en une chaîne YAML.

### Désérialisation de YAML en Structures Rust

Définissez une structure Rust qui reflète les données que vous attendez en YAML. Utilisez les attributs Serde pour la personnalisation si nécessaire.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Bouclier
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

Le résultat obtenu en exécutant le code Rust ci-dessus sera :

```plaintext
Config { name: "Bouclier", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Sérialisation des Structures Rust en YAML

Cet exemple prend la structure `Config` de la section précédente et la sérialise de nouveau au format YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Hache"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

Le résultat attendu sera une chaîne formatée en YAML :

```yaml
---
name: Hache
durability: 120
owner:
  name: Alex
  age: 30
```

Ces extraits démontrent comment intégrer efficacement l'analyse et la génération de YAML dans vos applications Rust, en utilisant les crates populaires `serde` et `serde_yaml`, pour accommoder des structures de données complexes et fournir des configurations simples et lisibles pour l'humain.
