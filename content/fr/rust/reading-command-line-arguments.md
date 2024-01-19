---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La lecture des arguments de ligne de commande permet d'adapter le comportement d'un programme selon les données spécifiées par l'utilisateur au moment de l'exécution. C'est un moyen efficace et polyvalent de contrôler les paramètres d'exécution.

## Comment faire :

En Rust, on utilise le module `std::env` pour lire les arguments de ligne de commande. Voici un exemple de base :

```Rust
fn main() {
    for argument in std::env::args() {
        println!("{}", argument);
    }
}
```

Si vous exécutez ce programme avec `cargo run arg1 arg2`, la sortie sera :

```Output 
target/debug/procject
arg1
arg2
```

## Plongée en profondeur :

L'appréhension des arguments de ligne de commande existe depuis les premiers jours de la programmation. Avant cela, les paramètres étaient généralement saisis manuellement ou codés en dur dans le programme.

Il existe plusieurs moyens d'interpréter les arguments de ligne de commande, `std::env::args()` est l'un des plus simples mais il existe des crates comme `clap` et `structopt` qui fournissent des fonctionnalités supplémentaires.

Il convient de noter que `std::env::args()` retourne un itérateur sur les arguments. Le premier élément de cet itérateur est toujours le chemin d'accès au programme, et les éléments suivants sont les arguments qui ont été passés à celui-ci.

## Voir aussi :

Pour plus d'informations, je vous recommande les ressources suivantes :
- Le livre de programmation Rust : https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html
- Documentation du module std::env : https://doc.rust-lang.org/std/env/index.html
- Crates pour l'analyse des arguments de ligne de commande : https://crates.io/crates/clap, https://crates.io/crates/structopt