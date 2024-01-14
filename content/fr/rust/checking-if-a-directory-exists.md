---
title:                "Rust: Vérifier l'existence d'un répertoire."
simple_title:         "Vérifier l'existence d'un répertoire."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi 

Dans le processus de développement logiciel, il est souvent nécessaire de vérifier si un répertoire existe avant d'effectuer une action spécifique. En utilisant le langage de programmation Rust, il est possible de le faire de manière efficace et fiable. Dans cet article, nous allons expliquer pourquoi cette vérification est importante et comment la réaliser en utilisant Rust.

## Comment faire

En utilisant la bibliothèque standard de Rust, il est très facile de vérifier si un répertoire existe. Tout d'abord, nous devons importer la bibliothèque `std::fs` à l'aide de la déclaration `use` suivante :

```Rust
use std::fs;
```

Ensuite, pour vérifier si un répertoire existe, nous utilisons la fonction `metadata()` de la bibliothèque `std::fs` en lui passant le chemin du répertoire à vérifier en paramètre. Si la fonction renvoie une erreur, cela signifie que le répertoire n'existe pas. Sinon, cela signifie que le répertoire existe et nous pouvons procéder à l'action souhaitée. Voici un exemple de code :

```Rust
use std::fs;

fn main() {
    if let Err(_) = fs::metadata("/chemin/vers/le/répertoire") {
        println!("Le répertoire n'existe pas.");
    } else {
        println!("Le répertoire existe.");
    }
}
```

Dans cet exemple, nous utilisons le mot-clé `if let` pour gérer le résultat de la fonction `metadata()` qui renvoie un type `Result`, soit une erreur, soit un succès.

## Plongée en profondeur

La fonction `metadata()` renvoie un type `Result` qui est un type énuméré avec deux variantes : `Ok` (succès) et `Err` (erreur). Pour gérer ces deux valeurs, on peut utiliser le mot-clé `match` qui nous permet d'exécuter du code spécifique en fonction de la valeur renvoyée par la fonction. Voici un exemple :

```Rust
use std::fs;

fn main() {
    match fs::metadata("/chemin/vers/le/répertoire") {
        Ok(_) => println!("Le répertoire existe."),
        Err(_) => println!("Le répertoire n'existe pas."),
    }
}
```

Dans cet exemple, nous n'avons pas besoin d'utiliser le mot-clé `let` car nous n'avons pas besoin de stocker la valeur renvoyée par la fonction `metadata()`.

Il est également possible d'utiliser la fonction `fs::create_dir()` pour créer un répertoire s'il n'existe pas déjà. Voici un exemple :

```Rust
use std::fs;

fn main() {
    if let Err(_) = fs::create_dir("/chemin/vers/le/répertoire") {
        println!("Impossible de créer le répertoire.");
    } else {
        println!("Le répertoire a été créé.");
    }
}
```

## Voir aussi

- [La bibliothèque standard de Rust](https://doc.rust-lang.org/std/)
- [La gestion des erreurs en Rust](https://doc.rust-lang.org/book/ch09-00-error-handling.html)