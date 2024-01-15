---
title:                "Écrire un fichier texte"
html_title:           "Rust: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous vous êtes déjà demandé comment les programmes informatiques créent des fichiers texte, cet article est pour vous. Apprendre à écrire un fichier texte en Rust vous aidera à mieux comprendre le fonctionnement de l'informatique et à élargir vos connaissances en programmation.

## Comment faire 

Pour écrire un fichier texte en Rust, nous allons utiliser la bibliothèque standard `std::fs`. Tout d'abord, nous devons ouvrir un fichier en mode écriture avec la fonction `File::create()`, en lui passant le nom du fichier en tant qu'argument. Ensuite, nous allons utiliser la fonction `write_all()` pour écrire notre texte dans le fichier. Voici un exemple de code:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("mon_fichier.txt").expect("Impossible d'ouvrir le fichier !");
    file.write_all(b"Bonjour, monde !").expect("Impossible d'écrire dans le fichier !");
}
```
Vous pouvez maintenant exécuter ce code en utilisant la commande `cargo run` et vous verrez un nouveau fichier nommé "mon_fichier.txt" dans le même répertoire que votre fichier source. Si vous ouvrez ce fichier, vous devriez voir le texte "Bonjour, monde !" écrit à l'intérieur.

## Plongeons plus en profondeur 

La fonction `write_all()` prend un argument de type `&[u8]` qui est une collection d'octets représentant notre texte. Cela signifie que nous devons convertir notre texte en octets avant de l'utiliser. Dans cet exemple, nous utilisons le préfixe `b` pour déclarer une chaîne littérale en tant que tableau d'octets. Vous pouvez également utiliser la fonction `as_bytes()` pour convertir une chaîne de caractères en tableau d'octets.

Il est également important de noter que la fonction `write_all()` remplace tout ce qui était déjà écrit dans le fichier. Si vous voulez ajouter du texte sans remplacer celui qui existe déjà, vous pouvez utiliser la fonction `File::append()` avant d'écrire.

## Voir aussi 

- [Documentation de la bibliothèque standard de Rust pour la manipulation de fichiers](https://doc.rust-lang.org/std/fs/index.html)
- [Tutoriel vidéo sur la manipulation de fichiers en Rust](https://www.youtube.com/watch?v=rg3nGoaboUI)
- [Exemples pratiques d'écriture de fichiers en Rust](https://www.oreilly.com/library/view/rust-programming-by/9781788390637/ce21df98-48e6-4d3b-bec3-4a7932757a35.xhtml)