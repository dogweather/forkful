---
title:    "Rust: Lecture d'un fichier texte"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte en Rust ?

Si vous êtes intéressé par la programmation en Rust, vous vous demandez peut-être pourquoi vous devriez même envisager de lire un fichier texte en utilisant ce langage. Eh bien, il y a plusieurs raisons pour lesquelles cela pourrait être une bonne idée !

## Comment faire ?

Tout d'abord, il est important de comprendre comment lire un fichier texte en Rust. Heureusement, c'est relativement simple avec l'aide de la bibliothèque standard de Rust. Voici un exemple de code pour lire un fichier texte et afficher son contenu :

```Rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut file = File::open("fichier.txt").expect("Impossible de trouver le fichier.");
    let mut contenu = String::new();

    file.read_to_string(&mut contenu)
        .expect("Impossible de lire le fichier.");

    println!("Contenu du fichier : \n{}", contenu);
}
```

Et voici le résultat que vous obtiendrez :

```
Contenu du fichier :
Bienvenue dans mon programme de lecture de fichiers en Rust !
```

Comme vous pouvez le voir, la bibliothèque standard de Rust fournit des fonctions utiles telles que `File::open` et `Read::read_to_string` pour faciliter la lecture des fichiers texte.

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte en Rust, vous pourriez vous demander comment gérer certains cas d'utilisation plus complexes. Par exemple, comment lire un fichier avec des encodages différents ? Ou comment gérer les erreurs de lecture de fichier ?

Pour ces questions et d'autres encore, il est utile de se plonger en profondeur dans la bibliothèque standard de Rust et d'explorer toutes les possibilités qu'elle offre pour la lecture de fichiers texte. N'hésitez pas à consulter la [documentation officielle](https://doc.rust-lang.org/std/fs/struct.File.html) pour en savoir plus.

# Voir aussi

- [Documentation officielle de la bibliothèque standard de Rust](https://doc.rust-lang.org/std/index.html)
- [Exemples de lecture de fichiers en Rust](https://github.com/rust-lang/rust-by-example#file-io)
- [Guide pratique pour la lecture et l'écriture de fichiers en Rust](https://stevedonovan.github.io/rust-gentle-intro/6-files.html)