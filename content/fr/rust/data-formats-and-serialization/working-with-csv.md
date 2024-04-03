---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:17.838296-07:00
description: "Travailler avec des fichiers CSV (Comma-Separated Values, soit valeurs\
  \ s\xE9par\xE9es par des virgules) consiste \xE0 lire et \xE0 \xE9crire dans des\
  \ fichiers texte\u2026"
lastmod: '2024-03-13T22:44:57.524709-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Comma-Separated Values, soit valeurs s\xE9\
  par\xE9es par des virgules) consiste \xE0 lire et \xE0 \xE9crire dans des fichiers\
  \ texte simples qui stockent des donn\xE9es tabulaires."
title: Travailler avec CSV
weight: 37
---

## Quoi & Pourquoi ?
Travailler avec des fichiers CSV (Comma-Separated Values, soit valeurs séparées par des virgules) consiste à lire et à écrire dans des fichiers texte simples qui stockent des données tabulaires. Les programmeurs font cela pour permettre le partage de données entre différents programmes, systèmes, ou pour traiter des ensembles de données volumineux de manière efficace et lisible par l'homme.

## Comment faire :
Rust, avec son accent sur la sécurité et la performance, offre d'excellentes crates (bibliothèques) pour manipuler des fichiers CSV, `csv` étant la plus populaire. Vous aurez également besoin de `serde` pour la sérialisation et la désérialisation des données.

D'abord, ajoutez les dépendances à votre `Cargo.toml` :

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Lire un CSV

Pour lire un fichier CSV, définissez une struct qui représente vos données et dérivez `Deserialize` de `serde` :

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("erreur lors de l'exécution de l'exemple : {}", err);
        process::exit(1);
    }
}
```

Un exemple de sortie pour un CSV avec des informations sur les villes pourrait ressembler à :
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Écrire dans un CSV

Pour écrire dans un fichier CSV, définissez une struct et dérivez `Serialize` :

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

Cela créera `output.csv` avec les données :

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

En exploitant le système de types puissant de Rust et les crates robustes de l'écosystème, travailler avec des données CSV devient à la fois efficace et simple, garantissant sécurité et performance dans vos tâches de traitement de données.
