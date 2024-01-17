---
title:                "Vérifier si un répertoire existe"
html_title:           "Rust: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Vérifier si un répertoire existe signifie simplement de vérifier si un dossier spécifique se trouve sur un système de fichiers. Les programmeurs le font pour s'assurer que leur code peut accéder à des fichiers ou des sous-dossiers avant de les manipuler.

## Comment faire:

```Rust
use std::path::Path;

let directory = Path::new("/chemin/vers/dossier");

if directory.exists() {
    println!("Le dossier existe !");
} else {
    println!("Le dossier n'existe pas.");
}
```

## Plongée en profondeur:

- Contexte historique: La vérification de l'existence de répertoires est une fonctionnalité de base dans la programmation depuis de nombreuses années.
- Alternatives: Il existe plusieurs façons de vérifier si un répertoire existe, telles que la création de fonctions personnalisées ou l'utilisation de bibliothèques tierces.
- Détails de mise en œuvre: En utilisant la bibliothèque standard de Rust, la fonction "exists()" utilise essentiellement un appel système pour vérifier si un chemin de fichier est valide.

## Voir aussi:

- [Documentation de la bibliothèque standard de Rust pour la vérification de l'existence de répertoires](https://doc.rust-lang.org/std/fs/fn.metadata.html#method.exists)
- [Article sur la gestion des fichiers et des répertoires en Rust](https://medium.com/@aidanhs/rust-ownership-and-the-filesystem-part-1-e48e19285991)
- [Exemples de code pour la vérification de l'existence de répertoires en Rust](https://github.com/gamache/check-dir-exists)