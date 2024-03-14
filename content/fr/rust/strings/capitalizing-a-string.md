---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:23.170611-07:00
description: "Mettre en majuscule la premi\xE8re lettre d'une cha\xEEne de caract\xE8\
  res en Rust implique de modifier cette cha\xEEne afin que son premier caract\xE8\
  re soit une\u2026"
lastmod: '2024-03-13T22:44:57.464555-06:00'
model: gpt-4-0125-preview
summary: "Mettre en majuscule la premi\xE8re lettre d'une cha\xEEne de caract\xE8\
  res en Rust implique de modifier cette cha\xEEne afin que son premier caract\xE8\
  re soit une\u2026"
title: "Mettre en majuscule une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Mettre en majuscule la première lettre d'une chaîne de caractères en Rust implique de modifier cette chaîne afin que son premier caractère soit une majuscule s'il s'agit d'une lettre, tout en laissant le reste de la chaîne inchangé. Les programmeurs effectuent souvent cette opération à des fins de formatage, comme préparer des mots pour des titres ou assurer la cohérence dans les entrées utilisateur.

## Comment faire :

Pour mettre en majuscule la première lettre d'une chaîne de caractères en Rust, vous avez deux options principales : utiliser les fonctionnalités de la bibliothèque standard ou employer des crates tierces pour des besoins plus complexes ou spécifiques. Voici comment procéder dans les deux cas.

### Utiliser la bibliothèque standard de Rust

La bibliothèque standard de Rust ne propose pas de méthode directe pour mettre en majuscule les chaînes de caractères, mais vous pouvez y parvenir en manipulant les caractères de la chaîne.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Résultat : Hello
}
```

### Utiliser la crate `heck`

Pour une approche plus directe, surtout lorsque vous travaillez dans un contexte de traitement de texte plus vaste, vous pourriez préférer utiliser des bibliothèques tierces telles que `heck`. La crate `heck` offre diverses fonctionnalités de conversion de cas, y compris un moyen simple de mettre en majuscule les chaînes de caractères.

Tout d'abord, ajoutez `heck` à votre `Cargo.toml` :

```toml
[dependencies]
heck = "0.4.0"
```

Ensuite, utilisez-la pour mettre en majuscule votre chaîne :

```rust
extern crate heck; // Non nécessaire dans l'édition Rust 2018 ou ultérieure
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Résultat : Hello World
}
```

Note : La méthode `to_title_case` fournie par `heck` met en majuscule chaque mot de la chaîne, ce qui pourrait être plus que ce que vous recherchez si vous souhaitez uniquement que le premier caractère de la chaîne soit en majuscule. Ajustez votre utilisation selon vos besoins spécifiques.
