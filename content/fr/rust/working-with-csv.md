---
title:                "Rust: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

CSV (Comma-separated values) est un format de fichier couramment utilisé pour stocker des données tabulaires. En tant que programmeur en Rust, apprendre à travailler avec des fichiers CSV peut être très utile pour manipuler et analyser des données.

## Comment faire

Pour commencer à travailler avec des fichiers CSV en Rust, vous pouvez utiliser la crate intégrée `csv`. Il s'agit d'une bibliothèque de haut niveau qui facilite la lecture et l'écriture de fichiers CSV. Voici un exemple de code pour lire et afficher les données d'un fichier CSV :

```Rust
extern crate csv;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut reader = csv::Reader::from_path("data.csv")?;
    for result in reader.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Le code utilise la fonction `from_path()` pour ouvrir le fichier CSV et la méthode `records()` pour itérer à travers chaque ligne du fichier. La variable `record` représente chaque ligne sous forme de vecteur, ce qui vous permet de travailler avec les données de votre choix. Vous pouvez également utiliser la méthode `write_record()` pour écrire des données dans un nouveau fichier CSV.

## Plongée en profondeur

Si vous souhaitez avoir un contrôle accru sur la manipulation de fichiers CSV, vous pouvez utiliser la crate `rust-csv`, qui propose une interface de bas niveau pour lire et écrire des fichiers CSV. Cette crate vous permet de définir des options de délimiteur, de gérer les en-têtes et les lignes vides, ainsi que de gérer les erreurs de parsing.

Il est également important de prendre en compte la performance lors de la manipulation de fichiers CSV en Rust. Il est recommandé d'utiliser la méthode `as_slice()` pour convertir les données du fichier en tranches (`slice`) avant de les enregistrer ou de les travailler, afin d'optimiser les performances.

## Voir aussi

- [Documentation de la crate `csv`](https://docs.rs/csv)
- [Documentation de la crate `rust-csv`](https://docs.rs/rust-csv)
- [Le guide officiel de Rust sur la manipulation de données CSV](https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html)