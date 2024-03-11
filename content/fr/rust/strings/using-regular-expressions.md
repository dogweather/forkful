---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:08.419765-07:00
description: "Les expressions r\xE9guli\xE8res, ou regex, permettent aux d\xE9veloppeurs\
  \ de rechercher, faire correspondre et manipuler des cha\xEEnes de caract\xE8res\
  \ avec des\u2026"
lastmod: '2024-03-11T00:14:31.487243-06:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res, ou regex, permettent aux d\xE9veloppeurs\
  \ de rechercher, faire correspondre et manipuler des cha\xEEnes de caract\xE8res\
  \ avec des\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières, ou regex, permettent aux développeurs de rechercher, faire correspondre et manipuler des chaînes de caractères avec des techniques avancées de reconnaissance de motifs. En Rust, l'utilisation de regex aide à analyser et gérer les données textuelles de manière efficace, rendant des tâches comme la validation de données, la recherche, et les transformations de texte plus fluides et maintenables.

## Comment faire :

La bibliothèque `regex` de Rust est un outil de choix pour travailler avec les expressions régulières. Pour l'utiliser, vous devrez d'abord l'ajouter à votre `Cargo.toml` :

```toml
[dependencies]
regex = "1"
```

Ensuite, vous pouvez commencer à implémenter des fonctionnalités regex dans votre code Rust. Voici comment effectuer certaines opérations communes :

### Correspondance à un motif dans une chaîne

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("Le texte correspond-il au modèle de date ? {}", re.is_match(date));
    // Sortie : Le texte correspond-il au modèle de date ? true
}
```

### Trouver et accéder aux correspondances

```rust
use regex::Regex;

fn main() {
    let texte = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(texte) {
        println!("Langage : {}, Année : {}", &cap[1], &cap[2]);
    }
    // Sortie :
    // Langage : Rust, Année : 2023
    // Langage : C++, Année : 2022
    // Langage : Python, Année : 2021
}
```

### Remplacer du texte

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let texte = "Rust 2023, C++ 2022, Python 2021";
    let texte_remplacé = re.replace_all(texte, "$1 a été mis à jour en $2");

    println!("Texte mis à jour : {}", texte_remplacé);
    // Sortie : Texte mis à jour : Rust a été mis à jour en 2023, C++ a été mis à jour en 2022, Python a été mis à jour en 2021
}
```

### Diviser du texte en utilisant une regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // diviser à tout caractère non-mot
    let texte = "Rust-C++-Python-Go";

    let champs: Vec<&str> = re.split(texte).collect();

    for champ in champs {
        println!("Langage : {}", champ);
    }
    // Sortie :
    // Langage : Rust
    // Langage : C++
    // Langage : Python
    // Langage : Go
}
```

Ces exemples fournissent un guide de base pour commencer avec les expressions régulières en Rust. Au fur et à mesure que vos besoins deviennent plus sophistiqués, la `crate regex` offre une multitude de fonctionnalités pour des tâches complexes de reconnaissance de motifs et de manipulation de texte.
