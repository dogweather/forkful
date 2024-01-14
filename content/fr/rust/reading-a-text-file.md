---
title:    "Rust: Lecture d'un fichier texte"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte peut sembler être une tâche simple et banale, mais en réalité, il y a beaucoup de choses intéressantes et utiles à apprendre lorsqu'on utilise Rust pour le faire. Dans cet article, nous allons explorer les différentes façons de lire un fichier texte en utilisant Rust, ainsi que les avantages que cela peut apporter à votre programmation.

## Comment faire

La première étape pour lire un fichier texte en Rust est de créer un objet de type `File`, qui représente le fichier lui-même. Pour cela, nous allons utiliser la fonction `open` de la bibliothèque standard `std::fs`.

```
Rust
fn main() {
    let file = std::fs::open("texte.txt").expect("Impossible de trouver le fichier");
    // Le fichier est maintenant stocké dans l'objet `file`
}
```

Une fois que nous avons l'objet `File`, nous pouvons l'utiliser pour lire le contenu du fichier. Pour cela, nous allons utiliser la fonction `read_to_string` de la bibliothèque standard `std::fs`, qui lit tout le contenu du fichier en une seule fois et le retourne sous forme de chaîne de caractères.

```
Rust
fn main() {
    let contenu = std::fs::read_to_string(file).expect("Impossible de lire le fichier");
    println!("{}", contenu);
}
```

Avec cette méthode, nous pouvons facilement lire le contenu d'un fichier texte et l'utiliser dans notre programme. Cependant, il existe d'autres façons de lire un fichier texte en utilisant Rust, comme lire le fichier ligne par ligne ou même en utilisant des itérateurs.

## Plongée en profondeur

Une fois que vous avez maîtrisé les bases de la lecture de fichiers texte en utilisant Rust, vous pouvez commencer à explorer les fonctionnalités avancées telles que la manipulation de l'encodage du fichier, la gestion des erreurs et la performance.

En utilisant les bons types de données et en gérant correctement les erreurs, vous pouvez écrire du code robuste et efficace pour lire des fichiers texte en Rust. De plus, en utilisant les bibliothèques appropriées, vous pouvez étendre les fonctionnalités de base pour faciliter la lecture de fichiers dans des formats spécifiques.

## Voir aussi

- [Documentation sur la lecture de fichiers en Rust](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Exemples de lecture de fichiers en Rust](https://www.tutorialspoint.com/how-to-read-and-write-a-file-in-rust-programming-language)
- [Bibliothèque `encoding_rs` pour gérer l'encodage des fichiers](https://crates.io/crates/encoding_rs)