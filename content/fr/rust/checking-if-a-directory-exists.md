---
title:    "Rust: Vérifier l'existence d'un dossier"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de vérifier si un répertoire existe avant d'effectuer certaines actions sur ce répertoire, afin d'éviter les erreurs et les problèmes de performance potentiels lors de l'exécution du code.

## Comment faire

Pour vérifier si un répertoire existe en utilisant le langage de programmation Rust, vous pouvez utiliser la fonction `std::fs::metadata()` qui renvoie un objet `std::fs::Metadata` contenant des informations sur le répertoire.

```Rust
use std::fs;

let metadata = fs::metadata("chemin_vers_le_répertoire");
if metadata.is_ok() && metadata.unwrap().is_dir() {
    println!("Le répertoire existe !");
} else {
    println!("Le répertoire n'existe pas !");
}
```

Dans cet exemple, nous utilisons également la méthode `.is_dir()` pour vérifier si le fichier est un répertoire ou non.

## Plongée en profondeur

La fonction `std::fs::metadata()` renvoie une erreur si le répertoire n'existe pas. Vous pouvez utiliser cette erreur pour créer le répertoire si nécessaire en utilisant la méthode `.create_dir()`.

```Rust
use std::fs;

let metadata = fs::metadata("chemin_vers_le_répertoire");
if metadata.is_ok() {
    // Le répertoire existe déjà
    if metadata.unwrap().is_dir() {
        println!("Le répertoire existe !");
    } else {
        // Le chemin pointe vers un fichier et non un répertoire
        println!("Un fichier existe déjà à cet emplacement !");
    }
} else {
    // Le répertoire n'existe pas, on peut le créer
    fs::create_dir("chemin_vers_le_répertoire").expect("Impossible de créer le répertoire !");
}
```

## Voir aussi

- [`std::fs` documentation (en anglais)](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial officiel de Rust (en français)](https://doc.rust-lang.org/book/title-page.html)