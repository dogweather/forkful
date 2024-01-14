---
title:                "Rust: Ecrire un fichier texte"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

On peut avoir plusieurs raisons de vouloir écrire un fichier texte en Rust. Peut-être que vous cherchez à stocker des données de manière persistante ou à générer automatiquement des rapports pour votre application. Quelle que soit la raison, écrire un fichier texte peut être utile dans de nombreuses situations et Rust offre une syntaxe simple et efficace pour le faire.

## Comment faire

Pour écrire un fichier texte en Rust, nous allons utiliser le module standard `std::fs` qui fournit des fonctions pour interagir avec le système de fichiers. Tout d'abord, nous devons ouvrir un fichier en mode écriture à l'aide de la fonction `File::create()`. Cette fonction prend en paramètre le chemin du fichier que nous voulons créer et renvoie un objet `File` qui représente notre fichier.

Ensuite, nous pouvons utiliser la méthode `write()` pour écrire des données dans notre fichier. Cette méthode prend en paramètre une référence à un vecteur de bytes qui représente les données que nous voulons écrire. Nous pouvons également utiliser la méthode `write_all()` qui prend en compte les erreurs lors de l'écriture de données.

Voici un exemple de code pour écrire un fichier texte en Rust :

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("mon_fichier.txt")?;
    file.write_all(b"Bonjour le monde!")?;

    Ok(())
}
```

Dans cet exemple, nous créons le fichier `mon_fichier.txt` et y écrivons la chaîne "Bonjour le monde!" en utilisant la méthode `write_all()`. N'oubliez pas d'ajouter `?` après chaque fonction pour gérer correctement les erreurs.

## Deep Dive

Maintenant que nous avons vu comment écrire un fichier texte en Rust, il est important de comprendre la différence entre écrire en mode texte et écrire en mode binaire. En mode texte, Rust utilise une implémentation UTF-8 pour encoder nos données en bytes, tandis qu'en mode binaire, les données sont écrites telles quelles sans conversion.

Il est également important de noter que les méthodes `write()` et `write_all()` ne sont pas les seules façons d'écrire des données dans un fichier en Rust. Il existe d'autres méthodes telles que `write_fmt()` pour formater les données avant de les écrire, ou `write_at()` pour spécifier la position exacte où les données doivent être écrites dans le fichier.

## Voir aussi

- Documentation officielle pour le module `std::fs` en Rust : https://doc.rust-lang.org/std/fs/
- Tutoriel pour écrire et lire des fichiers en Rust : https://deterministic.space/reading-and-writing-a-file-in-rust.html
- Exemples de code pour écrire des fichiers en mode texte et binaire : https://github.com/rust-lang/rust-by-example/blob/master/standard_library/file/create.md