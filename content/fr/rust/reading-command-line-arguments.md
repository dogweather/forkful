---
title:    "Rust: Lecture des arguments de ligne de commande"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi 

La lecture des arguments de ligne de commande peut sembler être une tâche simple, mais cela peut être très utile dans certains cas. C'est une bonne compétence à avoir en tant que programmeur Rust, car cela vous permet de créer des programmes qui peuvent être configurés et personnalisés par l'utilisateur.

## Comment Faire 

Pour lire les arguments de ligne de commande en Rust, nous utiliserons la bibliothèque standard `env`. Tout d'abord, nous devons importer le module dans notre code :

```Rust 
use std::env;
```

Ensuite, nous pouvons utiliser la fonction `args` pour récupérer tous les arguments de la ligne de commande :

```Rust 
let args: Vec<String> = env::args().collect();
```

Nous pouvons ensuite accéder aux arguments individuels en utilisant l'index correspondant :

```Rust 
let first_arg = args[0];
let second_arg = args[1];
// etc.
```

Pour obtenir le nombre total d'arguments, nous pouvons utiliser la fonction `len` :

```Rust 
let num_args = args.len();
```

Maintenant que nous avons récupéré les arguments, nous pouvons les utiliser dans notre programme en fonction de nos besoins.

## Plongée Profonde 

La fonction `args` retourne en fait un itérateur, ce qui signifie que nous pouvons utiliser toutes les méthodes d'itérateur sur les arguments, comme `map` et `filter`. De plus, nous pouvons également spécifier des arguments à l'exécution en utilisant la commande `cargo run -- <arguments>`.

Le type de données que nous avons utilisé pour stocker les arguments est un `Vec<String>`. Cela signifie qu'il s'agit d'un vecteur qui contient des chaînes de caractères. Si vous avez besoin d'arguments de types différents, vous pouvez également utiliser des vecteurs contenant d'autres types, comme `Vec<i32>` pour des arguments entiers.

## Voir Aussi 

- [Documentation officielle de la bibliothèque standard Rust - `env` module](https://doc.rust-lang.org/std/env/index.html)
- [Guide sur la lecture des arguments de ligne de commande en Rust](https://www.rust-lang.org/learn/cli)
- [Exemples de projets Rust utilisant la lecture des arguments de ligne de commande](https://github.com/rust-lang/rust/tree/master/examples/args)