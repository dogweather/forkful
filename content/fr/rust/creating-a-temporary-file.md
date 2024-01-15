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

## Pourquoi

Créer un fichier temporaire peut être utile pour stocker des données de manière temporaire, telles que des fichiers de cache ou des informations générées lors de l'exécution d'un programme.

## Comment faire

Pour créer un fichier temporaire en Rust, nous pouvons utiliser la fonction `std::fs::File::create`. Cette fonction prend en paramètre le chemin du fichier temporaire à créer et renvoie un objet `std::fs::File` qui peut être utilisé pour écrire des données dans le fichier. Voici un exemple de code :

```Rust
use std::fs::File;

fn main() {
    let temp_file = File::create("temp.txt").unwrap();
    // Fichier temporaire créé avec succès
}
```

Notez que la fonction `create` renvoie un `Result` qui peut être déballé à l'aide de la méthode `unwrap` pour gérer les éventuelles erreurs lors de la création du fichier temporaire.

Une fois que nous avons créé notre fichier temporaire, nous pouvons utiliser les méthodes du type `std::fs::File` pour écrire des données dans le fichier. Par exemple, nous pouvons utiliser la méthode `write` pour écrire une chaîne de caractères dans le fichier :

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut temp_file = File::create("temp.txt").unwrap();
    temp_file.write(b"Contenu du fichier temporaire").unwrap();
    // Données écrites avec succès
}
```

## Plongée en profondeur

Lors de la création d'un fichier temporaire en Rust, il est important de prendre en compte la sécurité et la gestion des erreurs. Pour assurer la sécurité, il est recommandé d'utiliser la macro `tempfile::Builder` qui offre de nombreuses fonctionnalités pour la gestion des fichiers temporaires. Voici un exemple de code utilisant cette macro :

```Rust
use tempfile::Builder;

fn main() {
    let temp_file = Builder::new().tempfile().unwrap();
    // Fichier temporaire créé avec succès
}
```

De plus, il est important de s'assurer que le fichier temporaire est supprimé après son utilisation. Pour cela, nous pouvons utiliser la méthode `tempfile::NamedTempFile::persist` qui renvoie un `TempPath` représentant le chemin du fichier temporaire à supprimer après sa fermeture.

## Voir aussi

- [Documentation officielle de Rust sur les fichiers](https://doc.rust-lang.org/std/fs/index.html)
- [Exemple de création de fichier temporaire avec la macro `tempfile::Builder`](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=dd56990dbe9b2b3cfc97c0cda880fb82)