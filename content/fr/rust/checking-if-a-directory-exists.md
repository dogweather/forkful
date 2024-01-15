---
title:                "Vérifier l'existence d'un répertoire"
html_title:           "Rust: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi?

Si vous êtes un développeur utilisant Rust, vous savez probablement à quel point la gestion des erreurs est importante dans ce langage. Le fait de vérifier si un répertoire existe avant de l'utiliser peut vous éviter des erreurs de runtime et vous aider à créer des applications plus robustes.

## Comment Faire?

Pour vérifier si un répertoire existe en utilisant Rust, nous pouvons utiliser la fonction `metadata()` du module `std::fs`. Cette fonction retourne un `Metadata` struct qui contient des informations sur le fichier ou le répertoire demandé. En utilisant la méthode `is_dir()`, nous pouvons vérifier si le chemin fourni mène à un répertoire existant.

```rust
use std::fs;

// Vérifie si le répertoire existe
let result = fs::metadata("/chemin/vers/mon/repertoire");
if result.is_ok() && result.unwrap().is_dir() {
    println!("Le répertoire existe !");
} else {
    println!("Le répertoire n'existe pas");
}
```

Dans l'exemple ci-dessus, nous utilisons la fonction `metadata()` pour obtenir des informations sur le répertoire spécifié. Nous vérifions ensuite si l'appel de la fonction a réussi en utilisant la méthode `is_ok()`. Si c'est le cas, nous utilisons la méthode `is_dir()` pour vérifier si le chemin fourni mène effectivement à un répertoire existant. Si c'est le cas, nous imprimons un message indiquant que le répertoire existe, sinon nous imprimons un message indiquant qu'il n'existe pas.

## Plongeons Plus Profondément

Il est important de noter que la méthode `metadata()` peut également être utilisée pour vérifier si un fichier existe. Si le chemin fourni mène à un fichier, la méthode `is_file()` renverra `true`. Si vous avez besoin de vérifier l'existence à la fois d'un répertoire et d'un fichier, vous pouvez utiliser la méthode `is_file()` en plus de `is_dir()`.

Il est également important de savoir que la fonction `metadata()` peut renvoyer une erreur si le fichier ou le répertoire n'existe pas ou si vous n'avez pas les permissions nécessaires pour y accéder. Il est donc important de gérer ces erreurs en utilisant des fonctions comme `unwrap()` ou `expect()`.

## Voir Aussi

- Le module `std::fs`: https://doc.rust-lang.org/std/fs/index.html
- La struct `Metadata` pour obtenir plus d'informations sur les fichiers et répertoires: https://doc.rust-lang.org/std/fs/struct.Metadata.html