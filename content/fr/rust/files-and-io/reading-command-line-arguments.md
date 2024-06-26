---
date: 2024-01-20 17:57:02.773083-07:00
description: "How to: En Rust, on utilise le crate `std::env` pour acc\xE9der aux\
  \ arguments de ligne de commande. Voici un exemple ."
lastmod: '2024-03-13T22:44:57.500360-06:00'
model: gpt-4-1106-preview
summary: "En Rust, on utilise le crate `std::env` pour acc\xE9der aux arguments de\
  \ ligne de commande."
title: Lecture des arguments de ligne de commande
weight: 23
---

## How to:
En Rust, on utilise le crate `std::env` pour accéder aux arguments de ligne de commande. Voici un exemple :

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        println!("Hello, {}", args[1]);
    } else {
        println!("Salut! Tu as oublié de fournir ton nom.");
    }
}
```

Si vous lancez ce programme avec `cargo run Jean`, la sortie serait :

```
Hello, Jean
```

Si aucun argument n'est donné:

```
Salut! Tu as oublié de fournir ton nom.
```

## Deep Dive:
Les arguments de ligne de commande existent depuis les débuts de l'informatique, rendant les logiciels utilisables dans différents contextes. En Rust, `std::env::args` renvoie un itérateur des arguments donnés au programme. La première valeur est toujours le chemin du programme lui-même, donc vos arguments commencent à l'index 1.

Alternativement, crate `clap` ou `structopt` permet une gestion avancée des arguments avec vérification d'erreurs et messages d'aide. Ils intègrent la possibilité de définir des options, des indicateurs, et des sous-commandes de manière déclarative.

La manipulation des arguments de ligne de commande implique souvent de gérer des erreurs, par exemple des arguments de format incorrect. Rust encourage la gestion active des erreurs à l'aide de `Result` plutôt que d'ignorer des cas d'erreur potentiels.

## See Also:
Pour plus d'informations, voici quelques liens utiles :
- Documentation Rust std::env : https://doc.rust-lang.org/std/env/
- Crates pour la gestion des arguments :
  - clap : https://crates.io/crates/clap
  - structopt : https://crates.io/crates/structopt
- Chapitre du livre "The Rust Programming Language" sur l'utilisation des arguments de la ligne de commande : https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html
