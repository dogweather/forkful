---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:43.796251-07:00
description: "Comment faire : Rust ne prend pas en charge YAML dans sa biblioth\xE8\
  que standard, donc nous utilisons couramment des crates tierces comme `serde` (pour\
  \ la\u2026"
lastmod: '2024-03-13T22:44:57.515347-06:00'
model: gpt-4-0125-preview
summary: "Rust ne prend pas en charge YAML dans sa biblioth\xE8que standard, donc\
  \ nous utilisons couramment des crates tierces comme `serde` (pour la s\xE9rialisation\
  \ et la d\xE9s\xE9rialisation des donn\xE9es) en combinaison avec `serde_yaml`."
title: Travailler avec YAML
weight: 41
---

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
