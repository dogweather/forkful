---
title:                "Rust: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Les arguments de ligne de commande sont un élément essentiel de la programmation Rust, permettant aux utilisateurs d'interagir avec un programme en passant des paramètres au moment de l'exécution. Comprendre comment lire ces arguments peut améliorer considérablement l'expérience du programme pour les utilisateurs finaux.

# Comment faire

La lecture des arguments de ligne de commande en Rust est facile grâce à la fonction "args" dans le module "std::env". Voici un exemple de code qui imprime les arguments passés lors de l'exécution du programme :

```Rust
use std::env;

fn main() {
    for arg in env::args() {
        println!("{}", arg);
    }
}
```

Pour exécuter ce programme avec des arguments, vous pouvez utiliser la commande suivante :

```bash
$ ./mon_programme argument1 argument2
```

Vous devriez voir l'affichage suivant :

```bash
argument1
argument2
```

# Plongée en profondeur

Alors que notre exemple précédent est simple, il y a plus à apprendre sur la lecture des arguments de ligne de commande en Rust. Par exemple, il est important de comprendre la différence entre "env::args" et "env::args_os", qui retourne une collection d'objets "OsString" plutôt que de "String". De plus, il est également possible de déterminer le nom du programme lui-même avec la fonction "args_os".

Pour en savoir plus sur la lecture des arguments de ligne de commande en Rust, vous pouvez consulter la documentation officielle : https://doc.rust-lang.org/std/env/index.html

# Voir aussi

- https://doc.rust-lang.org/book/ch12-00-an-io-project.html#reading-the-command-line-arguments
- https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html