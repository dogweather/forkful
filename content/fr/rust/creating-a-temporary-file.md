---
title:                "Rust: Création d'un fichier temporaire"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires peut sembler être une tâche simple, mais en réalité, elle peut être très utile lors de la programmation en Rust. Les fichiers temporaires peuvent être utilisés pour stocker des données temporaires ou pour tester certaines fonctionnalités sans avoir à modifier les fichiers existants. Dans cet article, nous allons explorer comment créer des fichiers temporaires en Rust et pourquoi cela peut être bénéfique pour votre code.

## Comment Faire

Pour créer un fichier temporaire en Rust, nous pouvons utiliser la fonction `tempfile::tempfile`. Cela créera un fichier temporaire dans le répertoire temporaire de votre système d'exploitation. Voici un exemple de code pour créer un fichier temporaire et y écrire du contenu:

```Rust
use std::io::Write;
use tempfile::tempfile;

let mut tmp_file = tempfile().expect("Failed to create temporary file");
write!(tmp_file, "Hello world!").expect("Failed to write to temporary file");
```

Ensuite, vous pouvez utiliser le fichier temporaire comme bon vous semble. Une fois que votre programme se termine, le fichier temporaire sera automatiquement supprimé par le système d'exploitation. Voici un exemple de sortie si nous imprimons le contenu du fichier temporaire créé dans l'exemple ci-dessus:

```Rust
use std::io::Read;

let mut output = String::new();
let mut tmp_file = tempfile().expect("Failed to create temporary file");
write!(tmp_file, "Hello world!").expect("Failed to write to temporary file");
tmp_file.read_to_string(&mut output).expect("Failed to read from temporary file");
println!("Output: {}", output); // Output: Hello world!
```

## Plongée Profonde

En créant des fichiers temporaires, vous aurez peut-être besoin de spécifier un certain nombre de paramètres, comme le préfixe, l'extension ou le répertoire où le fichier temporaire doit être créé. Pour cela, vous pouvez utiliser la fonction `tempfile::Builder` qui vous permet de personnaliser différents paramètres. Voici un exemple de code pour créer un fichier temporaire avec un préfixe spécifique et en spécifiant le répertoire de destination:

```Rust
use tempfile::Builder;

let tmp_file = Builder::new().prefix("rust_").tempfile_in("/my/directory").expect("Failed to create temporary file");
```

De plus, il est également possible de définir la taille maximale du fichier temporaire en utilisant la fonction `tempfile::Builder::max_size`. Cela peut être utile si vous avez besoin de stocker un grand volume de données temporaires. Enfin, si vous souhaitez que le fichier temporaire soit automatiquement supprimé même en cas d'erreur, vous pouvez utiliser la fonction `tempfile::Builder::suffix` qui ajoutera une chaîne de caractères au nom du fichier, ce qui le rendra unique et donc plus facile à supprimer.

## Voir Aussi

- La documentation officielle de [tempfile](https://docs.rs/tempfile).
- Le tutoriel sur [la gestion des fichiers et des dossiers en Rust](https://www.tutorialspoint.com/rust/rust_files_io.htm).
- L'article sur [la manipulation des fichiers et des répertoires en Rust](https://medium.com/@magnet2di/file-i-o-and-folder-manipulation-in-rust-ebe3b91a3da3).