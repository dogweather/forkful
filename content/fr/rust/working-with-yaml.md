---
title:                "Rust: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec YAML en Rust ?

YAML est un langage de sérialisation de données utilisé pour stocker et échanger des informations de manière structurée. Travailler avec YAML en Rust offre de nombreux avantages, notamment une syntaxe facile à lire et à écrire, ainsi que la possibilité d'incorporer des données complexes dans votre code.

## Comment faire ?

Avant de commencer à travailler avec YAML en Rust, vous devrez importer la bibliothèque YAML. Vous pouvez le faire en ajoutant `yaml = "0.4.3"` à votre section de dépendances dans le fichier `Cargo.toml`.

Ensuite, vous pouvez utiliser la méthode `serde_yaml::from_str()` pour convertir du contenu YAML en une structure de données en Rust. Voici un exemple de code pour lire un fichier YAML et afficher son contenu :

```rust
use std::fs::File;
use std::io::prelude::*;
use serde_yaml;

fn main() {
    // Ouvrir et lire le fichier YAML
    let mut fichier = File::open("exemple.yaml").expect("Impossible d'ouvrir le fichier");
    let mut contenu = String::new();
    fichier.read_to_string(&mut contenu).expect("Impossible de lire le fichier");

    // Convertir le contenu en une structure de données
    let donnees: Vec<Vec<String>> = serde_yaml::from_str(&contenu).expect("Impossible de lire le contenu du fichier");

    // Imprimer le contenu
    for ligne in donnees {
        println!("{:?}", ligne);
    }
}
```

En exécutant ce code, vous devriez voir le contenu de votre fichier YAML imprimé dans la console.

## Plongée en profondeur

Travailler avec YAML en Rust vous permet également de manipuler des données complexes telles que des structures imbriquées, des tableaux et des valeurs multiples pour une même clé. Voici un exemple de fichier YAML avec ces différents éléments :

```yaml
nom: Johanna
age: 32
amis:
  - nom: Marie
    age: 33
  - nom: Pierre
    age: 31
langages:
  - Rust
  - Python
```

En utilisant la méthode `serde_yaml::from_str()`, vous pouvez facilement accéder à ces données en Rust :

```rust
let contenu = "nom: Johanna
age: 32
amis:
  - nom: Marie
    age: 33
  - nom: Pierre
    age: 31
langages:
  - Rust
  - Python";

let donnees: Value = serde_yaml::from_str(&contenu).expect("Impossible de lire le contenu du fichier");

// Accéder aux valeurs
let nom = donnees["nom"].as_str().unwrap();
let age = donnees["age"].as_u64().unwrap();
let amis = donnees["amis"].as_array().unwrap();
let langages = donnees["langages"].as_array().unwrap();

println!("Nom: {}", nom);
println!("Age: {}", age);
println!("Amis: {:?}", amis);
println!("Langages: {:?}", langages);
```

## Voir aussi

- Documentation officielle de la bibliothèque YAML en Rust : https://crates.io/crates/yaml
- Tutoriel pour travailler avec YAML en Rust : https://dev.to/thedrummeraki/working-with-yaml-in-rust-3769
- Exemples de code pour utiliser la bibliothèque YAML en Rust : https://github.com/dtolnay/serde-yaml/tree/master/examples