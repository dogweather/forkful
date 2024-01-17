---
title:                "Lecture d'un fichier texte"
html_title:           "Rust: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lire un fichier texte en programmation est une tâche courante permettant de récupérer du contenu stocké dans un fichier et de l'utiliser dans nos programmes. Les programmeurs font cela pour interagir avec des données externes ou stockées de manière persistante afin de les manipuler ou de les utiliser dans leurs programmes.

## Comment faire:

Voici un exemple de code en Rust pour lire un fichier texte et afficher son contenu:

```
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("example.txt").expect("Impossible d'ouvrir le fichier.");

    let mut content = String::new();
    file.read_to_string(&mut content).expect("Impossible de lire le fichier.");

    println!("Contenu du fichier: {}", content);
}
```

Le fichier `example.txt` contiendrait par exemple le texte suivant:

```
Bonjour le monde!
```

Et l'exécution du programme donnerait l'output suivant:

```
Contenu du fichier: Bonjour le monde!
```

## Plongée en profondeur:

Lire des fichiers texte fait partie des tâches de base de la programmation depuis longtemps. Les alternatives possibles incluent l'utilisation de fichiers binaires ou de bases de données pour le stockage et la récupération de données. En termes d'implémentation, Rust utilise le trait `Read` pour lire des données à partir d'un flux et le module `std::fs` pour interagir avec des fichiers sur le système de fichiers.

## Voir aussi:

Pour plus d'informations sur la lecture de fichiers en Rust, consultez la documentation officielle sur les opérations sur les fichiers [ici](https://doc.rust-lang.org/std/fs/index.html). Vous pouvez également consulter des exemples de code sur la manipulation de fichiers dans le dépôt GitHub de Rust[ici](https://github.com/rust-lang/rust-by-example/tree/master/std_io/file).