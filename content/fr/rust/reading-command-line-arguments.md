---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Rust: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation, vous vous demandez peut-être pourquoi quelqu'un voudrait lire des arguments de ligne de commande. Eh bien, il s'avère que cette compétence peut être très utile pour créer des programmes plus interagissants avec les utilisateurs.

## Comment faire

Lorsque vous utilisez le langage de programmation Rust, lire des arguments de ligne de commande est assez simple. Tout d'abord, importez le module "std" et le module "env" pour accéder aux fonctions associées aux arguments de ligne de commande. Ensuite, utilisez la fonction "args" du module "env" pour récupérer une collection d'arguments. Vous pouvez ensuite itérer à travers cette collection pour utiliser les arguments dans votre programme.

Voici un exemple de code pour lire et afficher les arguments de ligne de commande en Rust :

```rust
use std::env;
 
fn main() {
    let args: Vec<String> = env::args().collect();
 
    for arg in args {
        println!("{}", arg);
    }
}
```

Si vous exécutez ce code avec la commande ```./my_program hello world```, vous obtiendrez la sortie suivante :

```
./my_program
hello
world
```

Notez que le premier argument est le nom du programme lui-même.

## Plongée en profondeur

Maintenant que vous savez comment lire des arguments de ligne de commande en Rust, vous pouvez utiliser cette compétence pour une variété de tâches. Par exemple, vous pouvez créer des programmes basés sur des paramètres spécifiques entrés par l'utilisateur, tels que la création d'un nouveau fichier avec un nom spécifié.

En utilisant des bibliothèques telles que "Clap" ou "Docopt", vous pouvez même créer des programmes avec une interface utilisateur plus avancée pour la gestion des arguments de ligne de commande.

## Voir aussi

- [Documentation officielle de Rust sur les arguments de ligne de commande](https://doc.rust-lang.org/std/env/index.html#args)
- [Bibliothèque Clap pour créer des programmes avec des arguments de ligne de commande](https://docs.rs/clap/2.33.3/clap/)  
- [Bibliothèque Docopt pour créer des programmes avec des arguments de ligne de commande](https://docs.rs/docopt/1.1.0/docopt/)