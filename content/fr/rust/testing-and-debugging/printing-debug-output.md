---
date: 2024-01-20 17:53:20.346733-07:00
description: "En Rust, afficher des informations de d\xE9bogage, c'est montrer l'\xE9\
  tat d'une variable ou ce qui se passe dans un programme. \xC7a aide les d\xE9veloppeurs\
  \ \xE0\u2026"
lastmod: 2024-02-19 22:05:16.312740
model: gpt-4-1106-preview
summary: "En Rust, afficher des informations de d\xE9bogage, c'est montrer l'\xE9\
  tat d'une variable ou ce qui se passe dans un programme. \xC7a aide les d\xE9veloppeurs\
  \ \xE0\u2026"
title: "Affichage des sorties de d\xE9bogage"
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi ?)
En Rust, afficher des informations de débogage, c'est montrer l'état d'une variable ou ce qui se passe dans un programme. Ça aide les développeurs à comprendre et à réparer les bugs rapidement.

## How to: (Comment faire :)
Rust fournit la macro `println!` pour afficher du texte et `dbg!` pour déboguer:

```Rust
fn main() {
    let greeting = "Bonjour le monde";
    // Affichage classique
    println!("Message: {}", greeting);

    // Débogage avec la macro dbg!
    let number = 42;
    dbg!(number);  // Affiche [src/main.rs:8] number = 42
}
```

Output:
```
Message: Bonjour le monde
[src/main.rs:8] number = 42
```

## Deep Dive (Plongée en profondeur)
L'affichage de débogage n'est pas nouveau; c'est une pratique vieille de plusieurs décennies. Rust a amélioré la méthode traditionnelle:

1. `println!` vs `dbg!`: `println!` est basique pour l'affichage; `dbg!` ajoute le fichier et la ligne d'où il est appelé.
2. Alternatives: Les crate de journalisation comme `log` et `env_logger` offrent plus de contrôle et sont mieux pour des projets importants.
3. Détails d'implémentation: `dbg!` retourne la propriété de la valeur, permettant de l'insérer dans des expressions sans perturber le flux du programme.

## See Also (Voir aussi)
- Le guide officiel sur l'impression (https://doc.rust-lang.org/rust-by-example/hello/print.html)
- La documentation de `std::fmt` pour le formatage avancé (https://doc.rust-lang.org/std/fmt/)
- Crates de journalisation pour Rust: `log` (https://crates.io/crates/log) et `env_logger` (https://crates.io/crates/env_logger)
