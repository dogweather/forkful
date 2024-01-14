---
title:    "Rust: Ecrire un fichier texte"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte avec Rust

Vous pourriez vous demander pourquoi vous devriez vous intéresser à écrire un fichier texte avec le langage de programmation Rust. Eh bien, la réponse est simple : Rust est un langage de programmation moderne, sécurisé et performant qui peut être utilisé pour une variété de tâches, y compris la manipulation de fichiers.

## Comment écrire un fichier texte avec Rust

Pour écrire un fichier texte en utilisant Rust, nous allons utiliser la bibliothèque standard `std::fs`. Voici un exemple de code qui crée un fichier texte appelé "mon_fichier.txt" et y écrit une ligne de texte :

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    // Créer le fichier
    let mut fichier = File::create("mon_fichier.txt")
        .expect("Impossible de créer le fichier");

    // Écrire une ligne de texte
    fichier
        .write_all(b"Bonjour, cher lecteur !\n")
        .expect("Impossible d'écrire dans le fichier");
}
```

Maintenant, si vous regardez le contenu du fichier "mon_fichier.txt", vous devriez voir la ligne de texte écrite dedans. Vous pouvez également utiliser le fichier dans vos programmes ultérieurs en utilisant `File::open()` pour ouvrir le fichier et `Read::lines()` pour lire le contenu ligne par ligne.

## Plongée en profondeur

Si vous voulez en savoir plus sur la manière dont Rust gère les fichiers, vous pouvez explorer davantage les capacités de la bibliothèque standard `std::fs`. Par exemple, il existe plusieurs méthodes pour vérifier l'état d'un fichier, comme `metadata()` qui renvoie des informations telles que la taille du fichier et ses autorisations.

Vous pouvez également consulter la bibliothèque externe `std::fs_extra` qui offre des fonctionnalités supplémentaires, telles que la copie et le déplacement de fichiers, et le package `serde` pour sérialiser et désérialiser des données en un fichier JSON.

# Voir aussi

- Documentation de la bibliothèque standard : https://doc.rust-lang.org/std/fs/index.html
- Bibliothèque `std::fs_extra` : https://crates.io/crates/fs_extra
- Package `serde` : https://crates.io/crates/serde