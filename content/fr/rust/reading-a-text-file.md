---
title:                "Rust: La lecture d'un fichier texte"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par l'apprentissage d'un nouveau langage de programmation ou si vous souhaitez simplement améliorer vos compétences dans un langage de haut niveau, la lecture de fichiers texte est une compétence fondamentale à maîtriser. Cela vous permettra de manipuler et traiter des données de manière efficace dans vos projets.

## Comment Faire

Pour lire un fichier texte en Rust, vous devez tout d'abord ouvrir le fichier en utilisant la fonction ```File::open``` qui prend en paramètre le chemin du fichier. Ensuite, vous pouvez utiliser la méthode ```read_to_string``` pour lire le contenu du fichier en tant que chaîne de caractères.

Voici un exemple de code qui lit un fichier texte nommé "mon_fichier.txt" :

```Rust
use std::fs::File;
use std::io::Read;

fn main() {
    // Ouvre le fichier et gère les erreurs en utilisant 'expect'
    let mut fichier = File::open("mon_fichier.txt").expect("Impossible d'ouvrir le fichier.");
    // Lit le contenu du fichier en tant que chaîne de caractères
    let mut contenu = String::new();
    fichier.read_to_string(&mut contenu).expect("Impossible de lire le fichier.");
    // Affiche le contenu du fichier
    println!("{}", contenu);
}
```

Lorsque vous exécuterez ce code, vous devriez voir le contenu du fichier imprimé dans votre terminal.

## Plongée Profonde

En utilisant la bibliothèque standard de Rust, vous pouvez également effectuer des opérations plus avancées sur les fichiers tels que la lecture d'une seule ligne à la fois avec la méthode ```lines()``` ou la lecture de données binaires avec la méthode ```read()```. Il est également possible de créer des fichiers en utilisant la méthode ```File::create()``` et d'autres opérations de gestion de fichiers.

Il est important de noter que la lecture de fichiers en Rust est particulièrement efficace grâce à l'utilisation de gestionnaires de mémoire sans garantie. Cela signifie que le compilateur Rust garantit qu'aucune donnée ne sera accédée après la fermeture d'un fichier, ce qui réduit le risque d'erreurs de programmation.

## Voir Aussi

- [Documentation de la bibliothèque standard de Rust](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Tutoriel sur la gestion des fichiers en Rust](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html#reading-a-file)
- [Exemples de code sur la lecture de fichiers en Rust](https://github.com/rust-lang/rust-by-example/std_misc/file/open.html)