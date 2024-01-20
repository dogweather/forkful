---
title:                "Travailler avec des fichiers csv"
html_title:           "Rust: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Travailler avec des fichiers CSV est une tâche courante pour les programmeurs. Les fichiers CSV sont des fichiers de données structurées, organisés sous forme de tableaux, avec les données séparées par des virgules. Les programmeurs utilisent ces fichiers pour stocker et manipuler des données de manière efficace.

## Comment faire:

Voici un exemple de code en utilisant le langage de programmation Rust pour lire un fichier CSV et afficher son contenu:

```Rust
extern crate csv; // Import de la bibliothèque csv

use std::error::Error;
use std::path::Path;

fn main() {
   let path = Path::new("mon_fichier.csv"); // Définir le chemin vers votre fichier CSV 
   let file = match File::open(path) { // Ouvrir le fichier
       Ok(file) => file,
       Err(why) => panic!("Impossible d'ouvrir le fichier : {}")
   };

   let mut reader = csv::Reader::from_reader(file); // Créer un lecteur pour lire le fichier CSV
   for result in reader.records() { // Parcourir chaque ligne du fichier
       let record = result.expect("Erreur lors de la lecture de la ligne");
       println!("{:?}", record[0]); // Afficher la première colonne de chaque ligne
   }
}
```

Le résultat de ce code est d'afficher la première colonne de chaque ligne du fichier CSV.

## Plongée profonde:

Les fichiers CSV sont utilisés depuis longtemps pour stocker et échanger des données. Ils sont simples et faciles à créer, et peuvent être lus par la plupart des programmes. Cependant, ils ne peuvent pas stocker des données complexes telles que des tableaux ou des objets.

Il existe d'autres formats de fichiers de données tels que JSON et XML, qui peuvent être utilisés pour stocker des données plus complexes. Cependant, ils peuvent être plus difficiles à lire et à manipuler que les fichiers CSV.

L'implémentation de CSV en Rust est basée sur la spécification officielle CSV RFC 4180. La bibliothèque Rust pour CSV fournit des fonctions pour lire, écrire et manipuler des fichiers CSV de manière efficace.

## Voir aussi:

- [Documentation officielle de la bibliothèque CSV pour Rust](https://docs.rs/csv/1.0.0/csv/)
- [Spécification officielle CSV RFC 4180](https://tools.ietf.org/html/rfc4180)