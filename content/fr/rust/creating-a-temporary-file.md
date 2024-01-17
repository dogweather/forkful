---
title:                "Créer un fichier temporaire"
html_title:           "Rust: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Créer un fichier temporaire en Rust

## Quoi & Pourquoi?
Créer un fichier temporaire est un moyen pour les programmeurs de générer un fichier qui sera utilisé temporairement pendant l'exécution d'un programme. Cela peut être utile lorsque vous avez besoin d'un fichier pour stocker des données temporaires ou pour réaliser des opérations de manière sécurisée.

## Comment faire:
Pour créer un fichier temporaire en Rust, vous pouvez utiliser la fonction `tempfile::tempfile()` de la bibliothèque standard. Cela renvoie un `Result<File>` qui peut être utilisé pour lire et écrire des données dans le fichier temporaire. Voici un exemple de code qui crée un fichier temporaire et y écrit une chaîne de caractères:

```Rust
use std::fs::File;
use std::io::prelude::*;

let mut temp_file = tempfile::tempfile().expect("Impossible de créer le fichier temporaire");
write!(temp_file, "Ceci est un fichier temporaire créé en Rust").expect("Impossible d'écrire dans le fichier temporaire");
```

## Plongée en profondeur:
La création de fichiers temporaires a été un sujet de discussion dans le monde de la programmation depuis un certain temps. Les programmeurs ont longtemps cherché des moyens de stocker des données de manière temporaire sans avoir à créer de fichier physique. En plus de la méthode mentionnée ci-dessus, il existe d'autres façons de créer des fichiers temporaires en Rust, comme l'utilisation de la bibliothèque `tempdir` ou en utilisant des bibliothèques tierces telles que `rusty-machine`.

## Voir aussi:
- [Documentation de la fonction tempfile() dans la bibliothèque standard](https://doc.rust-lang.org/std/fs/fn.tempfile.html)
- [Bibliothèque tempdir pour créer des répertoires temporaires en Rust](https://crates.io/crates/tempdir)
- [Bibliothèque rusty-machine pour la création de fichiers temporaires de manière sécurisée](https://crates.io/crates/rusty-machine)