---
title:                "Rust: Vérifier l'existence d'un répertoire"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Lors de la programmation, il est souvent nécessaire de vérifier si un dossier existe avant d'effectuer certaines actions. En utilisant Rust, un langage de programmation moderne et performant, vous pouvez facilement intégrer cette fonctionnalité à votre code.

## Comment faire

Pour vérifier si un dossier existe en Rust, vous pouvez utiliser la méthode `Path::exists()` de la bibliothèque standard. Elle renvoie un booléen indiquant si le chemin spécifié existe ou non.

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/chemin/vers/le/dossier"); // remplacez par le chemin que vous souhaitez vérifier

    if path.exists() {
        println!("Le dossier existe !");
    } else {
        println!("Le dossier n'existe pas.");
    }
}
```

Si vous préférez obtenir une réponse précise sur le type de fichier, vous pouvez utiliser la méthode `Path::is_dir()` pour vérifier si le chemin spécifié est bien un dossier.

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/chemin/vers/le/dossier"); // remplacez par le chemin que vous souhaitez vérifier

    if path.is_dir() {
        println!("Le chemin spécifié est bien un dossier !");
    } else {
        println!("Le chemin spécifié n'est pas un dossier.");
    }
}
```

## Plongée dans les détails

Lorsque vous utilisez la méthode `Path::exists()`, il est important de garder à l'esprit que cette méthode renvoie également `true` si le chemin spécifié correspond à un fichier. Si vous n'êtes pas certain que le chemin pointe vers un dossier, vous pouvez utiliser la méthode `Path::is_dir()` pour vous assurer que le chemin est bien un dossier.

Il est également possible de vérifier si un dossier existe à l'aide de la bibliothèque externe `std::fs`. Cette bibliothèque fournit une fonction `metadata()` qui renvoie des informations sur le fichier ou le dossier spécifié. Vous pouvez ensuite utiliser la méthode `is_dir()` sur ces informations pour vérifier si le chemin est bien un dossier.

## Voir aussi

- [Documentation officielle de Rust](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Article sur la vérification de l'existence d'un fichier en Rust](https://blog.ophir.dev/post/4/retour-sur-la-verifcation-de-lexistence-dans-un-dossier-en-rust)