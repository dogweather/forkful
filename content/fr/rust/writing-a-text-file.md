---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Écrire un fichier texte est l'action d'enregistrer des données sous forme de texte dans un fichier. Les programmeurs le font pour sauvegarder des configurations, des logs, ou pour générer des données exploitables par l'utilisateur ou d'autres programmes.

## How to:

```Rust
use std::fs::File;
use std::io::{Write, Result};

fn main() -> Result<()> {
    let mut file = File::create("exemple.txt")?;
    file.write_all(b"Ceci est un test d'écriture de fichier en Rust.")?;
    Ok(())
}
```
Ce code crée un fichier `exemple.txt` et y écrit la phrase "Ceci est un test d'écriture de fichier en Rust.".

## Deep Dive

Historiquement, l'accès aux fichiers en programmation est une opération de base essentielle. Rust offre une sécurité accrue pendant ces opérations grâce à son système de gestion des erreurs. Les alternatives incluent les bibliothèques comme `std::fs::write` qui simplifie le code en écrivant en une ligne ou `std::io::BufWriter` pour une écriture tamponnée efficace. La bonne mise en œuvre dépend des besoins spécifiques en performance et en traitement des erreurs.

## See Also

- [std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [std::io](https://doc.rust-lang.org/std/io/index.html)
