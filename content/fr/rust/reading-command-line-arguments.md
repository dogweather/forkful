---
title:                "Rust: Analyse d'arguments de ligne de commande"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lecture des arguments de ligne de commande est un aspect important de la programmation en Rust. Cela permet aux utilisateurs de passer des paramètres spécifiques lors de l'exécution d'un programme, ce qui peut rendre l'expérience plus personnalisée et flexible. Dans cet article, nous expliquerons comment lire et utiliser les arguments de ligne de commande en Rust.

## Comment faire

Pour lire des arguments de ligne de commande dans Rust, nous utiliserons la fonction "args()" de la bibliothèque standard. Elle renvoie un vecteur contenant tous les arguments passés lors de l'exécution du programme. Voici un exemple de code :

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Les arguments passés sont : {:?}", args);
}
```

Supposons que notre programme s'appelle "lecture_args", si nous l'exécutons de la manière suivante :

```
./lecture_args hello world 123
```

Nous obtiendrons l'output suivant :

```
Les arguments passés sont : ["./lecture_args", "hello", "world", "123"]
```

Nous pouvons également accéder à des arguments spécifiques en utilisant la méthode "get()" du vecteur. Par exemple, si nous voulons accéder au second argument (dans ce cas, "world"), le code serait le suivant :

```Rust
let deuxieme_arg = args.get(2).unwrap();
println!("Le deuxième argument est : {}", deuxieme_arg);
```

## Plongée en profondeur

La fonction "args()" de la bibliothèque standard est basée sur les arguments passés à la ligne de commande. Si nous voulons utiliser des arguments de manière plus complexe, comme les options et les valeurs, nous pouvons utiliser des bibliothèques externes telles que "clap" ou "structopt". Elles offrent des fonctionnalités avancées et une meilleure gestion des erreurs. Nous pouvons également convertir les arguments en différents types de données en utilisant des méthodes telles que "parse()", "to_owned()" ou "into()".

## Voir aussi

- [Documentation officielle de Rust sur la fonction "args()"](https://doc.rust-lang.org/std/env/fn.args.html)
- [Bibliothèque "clap" pour une gestion avancée des arguments](https://docs.rs/clap/2.33.1/clap/)
- [Bibliothèque "structopt" pour une meilleure gestion des erreurs](https://docs.rs/structopt/0.3.24/structopt/)