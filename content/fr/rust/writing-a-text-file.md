---
title:                "Ecrire un fichier texte"
html_title:           "Rust: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi faire?

Écrire un fichier texte en programmation est une tâche courante de stocker des données dans un format facile à lire et à manipuler. Les programmeurs le font souvent lorsque les données sont trop nombreuses ou complexes pour être stockées dans des variables ou des tableaux réguliers.

## Comment faire:

```Rust
use std::fs::File;  //pour ouvrir / créer un fichier
use std::io::prelude::*;  //pour écrire dans le fichier

fn main() { 
    let mut f = File::create("exemple.txt").unwrap();  //crée un nouveau fichier appelé "exemple txt"

    f.write_all(b"Bonjour le monde!").unwrap();  //écrit "Bonjour le monde!" dans le fichier

    //affiche une confirmation de la réussite de l'écriture
    println!("Fichier exemple.txt créé avec succès!"); 
}
```

## Plongée en profondeur:

Écrire des fichiers textes remonte aux débuts de la programmation, lorsque les données étaient stockées sur des rubans ou des cartes perforées. De nos jours, il existe des alternatives telles que des bases de données, mais les fichiers textes restent utiles pour leur simplicité et leur compatibilité avec de nombreux langages de programmation. L'implémentation peut varier selon les systèmes d'exploitation, mais la logique reste la même: ouvrir un fichier, écrire dedans et le fermer.

## Voir aussi:

- [Documentation officielle de Rust](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Article sur les avantages et les inconvénients des fichiers textes](https://www.bitdegree.org/learn/database-file)
- [Comparaison des différentes méthodes d'écriture de fichiers en Rust](https://users.rust-lang.org/t/efficiently-writing-a-large-text-file/12061/2)