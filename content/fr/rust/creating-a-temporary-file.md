---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi & Quoi ?
Créer un fichier temporaire consiste à générer un fichier que vous pouvez utiliser brièvement pendant l'exécution de votre programme. Les développeurs le font souvent pour conserver les données intermédiaires ou pour partager des données entre différents processus.

## Comment faire :
Voici comment vous créeriez et utiliseriez un fichier temporaire dans Rust :

```Rust
use std::fs::File;
use std::io::Write;
use tempfile::tempfile;

let mut temp = tempfile().expect("Impossible de créer un fichier temporaire");
write!(temp, "Salut Monde !").expect("Impossible d'écrire dans le fichier temporaire");

// Utilisez ici le fichier temporaire
```

## Plongée en profondeur
Historiquement, la création de fichiers temporaires a été une partie importante de la gestion de l'espace disque dans des langages comme C. Mais dans Rust, cela a été rendu beaucoup plus simple et sûr grâce au sûreté de la mémoire et des abstractions de gestion des fichiers. 

En alternative, vous pouvez aussi créer des dossiers temporaires avec le crate `tempdir`. Mais `tempfile` est généralement plus rapide et plus sûr car il garantit que le nom de fichier est unique et que le fichier est supprimé lorsque l'objet est abandonné.

L'implémentation de 'tempfile' en Rust utilise les appels systèmes spécifiques à chaque OS pour assurer la plus grande efficacité et sécurité possible. Par exemple, sur les systèmes Unix, il utilise `mkstemp` qui crée et ouvre le fichier en une seule opération atomique, évitant ainsi les attaques de type TOCTOU (time of check to time of use).

## Voir également
Pour plus d'informations et de ressources concernant les fichiers temporaires en Rust, consultez ces liens :
- Documentation Rust : https://doc.rust-lang.org/book/
- tempfile crate : https://docs.rs/tempfile/3.0.5/tempfile/
- Deuxième édition du livre Rust : https://doc.rust-lang.org/book/second-edition/