---
title:    "Rust: Créer un fichier temporaire"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est un outil essentiel pour les programmeurs Rust. Elle permet de stocker temporairement des données ou des informations, sans avoir à les enregistrer définitivement sur le système de fichiers. Cela peut être utile pour des opérations telles que la manipulation de données imparfaites ou la gestion de requêtes en cours.

## Comment faire

Pour créer un fichier temporaire en Rust, vous pouvez utiliser la fonction `tempfile::NamedTempFile`. Voici un exemple de code :

```Rust
use std::io::prelude::*;
use std::fs::File;
use tempfile::NamedTempFile;

let mut temp_file = match NamedTempFile::new() {
    Ok(file) => file,
    Err(e) => panic!("Impossible de créer un fichier temporaire: {}", e),
};

// Écrire du contenu dans le fichier temporaire
temp_file.write_all(b"Ceci est du contenu temporaire").expect("Impossible d'écrire dans le fichier temporaire");

// Récupérer un clone de l'objet de fichier
let file_clone = match temp_file.reopen() {
    Ok(file) => file,
    Err(e) => panic!("Impossible de réouvrir le fichier temporaire: {}", e),
};

// Imprimer le contenu du fichier temporaire
println!("Contenu du fichier : {}", file_clone.read_to_string().expect("Impossible de lire le contenu du fichier temporaire"));
```

Lorsque vous exécutez ce code, vous devriez obtenir l'impression suivante :

```
Contenu du fichier temporaire : Ceci est du contenu temporaire
```

## Plongée en profondeur

La fonction `NamedTempFile::new` crée un fichier temporaire vide dans le répertoire temporaire du système d'exploitation. Si vous voulez spécifier un nom de fichier précis, vous pouvez utiliser `NamedTempFile::new_in` en spécifiant le chemin d'accès au répertoire temporaire souhaité.

De plus, la fonction `tempfile::TempDir` peut également être utilisée pour créer un répertoire temporaire au lieu d'un fichier. Cela peut être utile si vous avez besoin de stocker plusieurs fichiers temporaires dans un même répertoire.

## Voir aussi

- [Documentation officielle de Rust pour la création de fichiers temporaires](https://doc.rust-lang.org/std/io/struct.TempDir.html)
- [Guide complet sur l'utilisation de fichiers temporaires en Rust](https://blog.yoshuawuyts.com/temp-dir/)