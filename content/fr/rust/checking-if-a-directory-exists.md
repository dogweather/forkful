---
title:    "Rust: Vérifier si un répertoire existe"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez du code en Rust, il est important de s'assurer que toutes les données utilisées sont correctes et accessibles. Une des vérifications les plus courantes est de vérifier si un répertoire existe avant d'essayer d'y accéder. Dans cet article, nous allons voir comment vérifier l'existence d'un répertoire en Rust avec des exemples de code.

## Comment faire

Il y a quelques façons différentes de vérifier si un répertoire existe en Rust, toutes avec des avantages et des inconvénients. Nous allons couvrir deux méthodes courantes: en utilisant la bibliothèque standard `std::fs` et en utilisant la bibliothèque de gestion de chemins `path`.

### Utiliser `std::fs`

La méthode la plus simple pour vérifier si un répertoire existe est d'utiliser la bibliothèque standard `std::fs`. En utilisant la fonction `metadata`, nous pouvons récupérer les métadonnées du fichier ou du répertoire spécifié. Si les métadonnées indiquent qu'il s'agit d'un répertoire, alors celui-ci existe.

```Rust
use std::fs;

fn main() {
    let dir_path = "/path/to/directory";
    let metadata = fs::metadata(dir_path);

    if metadata.is_ok() && metadata.unwrap().is_dir() {
        println!("Le répertoire existe!");
    } else {
        println!("Le répertoire n'existe pas.");
    }
}
```

Dans cet exemple, nous utilisons la fonction `is_ok()` pour vérifier si l'appel à `metadata` a réussi et la fonction `is_dir()` pour vérifier si les métadonnées correspondent à un répertoire. Si l'une ou l'autre de ces conditions n'est pas remplie, alors le répertoire n'existe pas.

Il est également possible d'utiliser la fonction `exists` pour vérifier directement si un répertoire existe, mais cette méthode n'est pas recommandée car elle peut générer des erreurs inattendues.

### Utiliser `path`

Une autre méthode pour vérifier l'existence d'un répertoire est d'utiliser la bibliothèque de gestion de chemins `path`. Cette méthode nécessite d'abord de créer un objet `Path` à partir du chemin du répertoire, puis de vérifier son existence à l'aide de la fonction `exists`.

```Rust
use std::path::Path;

fn main() {
    let dir_path = "/path/to/directory";

    if Path::new(dir_path).exists() {
        println!("Le répertoire existe!");
    } else {
        println!("Le répertoire n'existe pas.");
    }
}
```

Cette méthode est un peu plus verbeuse, mais elle offre des options supplémentaires pour manipuler les chemins et vérifier leur existence.

## Deep Dive

Maintenant que nous avons vu comment vérifier l'existence d'un répertoire en Rust, il est important de noter que cette opération ne garantit pas que le répertoire est accessible ou peut être utilisé. Il est également possible que le répertoire soit supprimé après avoir été vérifié. Pour cette raison, il est généralement recommandé d'associer cette vérification avec des opérations supplémentaires pour s'assurer que le répertoire peut être utilisé sans erreur.

De plus, il est important de noter que ces méthodes vérifient uniquement si le répertoire existe à un instant donné. Si le répertoire est créé ou supprimé pendant l'exécution du programme, ces méthodes peuvent ne plus être fiables. Dans ce cas, il est préférable de gérer les erreurs liées à l'accès au répertoire plutôt que de simplement vérifier son existence.

## Voir aussi

- Documentation de la bibliothèque standard de Rust pour [std::fs](https://doc.rust-lang.org/std/fs/index.html)
- Documentation de la bibliothèque de gestion de chemins de Rust pour [path](https://doc.rust-lang.org/std/path/index.html)
- Exemples de gestion de fichiers et de répertoires en Rust sur [GitHub](https://github.com/rust-lang/rust/tree/master/src/test)