---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:58:21.698213-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier l'existence d'un répertoire, c'est comme sonner à une porte pour s'assurer que quelqu'un est chez lui. Les programmeurs font ça pour éviter les erreurs : lire, écrire ou modifier un répertoire qui n'existe pas, c'est un non-sens.

## How to:
Pour vérifier si un répertoire existe en Rust, utilisez le module `std::fs` et la fonction `metadata()` ou `Path::exists()`.

```rust
use std::fs;
use std::path::Path;

fn main() {
    let path = Path::new("/chemin/vers/le/dossier");

    // Avec fs::metadata()
    match fs::metadata(path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Le répertoire existe!");
            } else {
                println!("C'est un fichier, pas un répertoire.");
            }
        },
        Err(e) => println!("Le répertoire n'existe pas: {}", e),
    }

    // Ou plus simplement avec Path::exists()
    if path.exists() {
        println!("Le répertoire existe!");
    } else {
        println!("Le répertoire n'existe pas.");
    }
}
```

Sortie possible :
```
Le répertoire existe!
Le répertoire n'existe pas.
```

## Deep Dive
Historiquement, vérifier l'existence d'un répertoire est essentiel depuis les débuts de la gestion des systèmes de fichiers. En Rust, `std::fs::metadata()` vous donne plus d'info, mais si vous voulez juste savoir si le répertoire existe, `Path::exists()` est plus direct et ergonomique.

Alternativement, pour plus de contrôle, `fs::metadata()` vous permet de vérifier si le chemin est un fichier ou un répertoire et d'accéder aux métadonnées. C'est utile pour des vérifications plus fines. Mais attention, vérifier l'existence puis lire ou écrire peut mener à une condition de compétition (_race condition_): entre la vérification et l'action, le statut peut changer suite à une action externe.

## See Also
Pour approfondir votre compréhension et explorer la documentation officielle de Rust:

- [`std::fs` module](https://doc.rust-lang.org/std/fs/index.html)
- [Trait `std::path::Path`](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Trait `Metadata`](https://doc.rust-lang.org/std/fs/struct.Metadata.html)
