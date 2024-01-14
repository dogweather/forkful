---
title:                "Rust: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Rust, vous pourriez vous demander pourquoi il serait important de lire un fichier texte. Eh bien, la lecture de fichiers texte est une tâche courante en programmation et c'est une compétence utile à avoir lorsque vous développez des applications ou des scripts.

## Comment faire

Pour lire un fichier texte en Rust, vous pouvez utiliser la fonction `read_to_string()` de la bibliothèque standard. Voici un exemple de code :

```Rust
use std::fs::File;
use std::io::prelude::*;
 
fn main() {
    let mut file = File::open("mon_fichier.txt").expect("Erreur lors de l'ouverture du fichier");
    let mut contenu = String::new();
    file.read_to_string(&mut contenu).expect("Erreur lors de la lecture du fichier");
    println!("Le contenu du fichier est :\n{}", contenu);
}
```

Dans cet exemple, nous utilisons la fonction `open()` pour ouvrir le fichier et nous vérifions si une erreur survient. Ensuite, nous utilisons `read_to_string()` pour lire le contenu du fichier dans une chaîne de caractères mutable et nous imprimons le contenu à l'aide de `println!()`.

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte en Rust, vous pourriez vous demander comment cela fonctionne réellement. En fait, la fonction `read_to_string()` utilise un itérateur qui lit le fichier par blocs et les insère dans la chaîne de caractères. Cela permet de lire des fichiers de taille importante sans avoir à stocker tout le contenu en mémoire.

De plus, la bibliothèque standard offre également d'autres fonctions pour lire des fichiers texte, comme `read()` qui permet de spécifier la taille du bloc à lire à chaque itération.

## Voir aussi

Voici quelques liens qui pourraient également vous être utiles pour en savoir plus sur la lecture de fichiers texte en Rust :

- [Documentation de la bibliothèque standard sur la lecture de fichiers](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Article de blog sur la lecture et l'écriture de fichiers en Rust](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-rust)
- [Exemples de code pour lire des fichiers ligne par ligne en Rust](https://www.programiz.com/rust-programming/file-input-output)