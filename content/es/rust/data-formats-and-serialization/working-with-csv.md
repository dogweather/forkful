---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:22.671528-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) se trata de\
  \ leer y escribir en archivos de texto plano que almacenan datos tabulares. Los\u2026"
lastmod: '2024-03-11T00:14:32.684955-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) se trata de leer\
  \ y escribir en archivos de texto plano que almacenan datos tabulares. Los\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con archivos CSV (Valores Separados por Comas) se trata de leer y escribir en archivos de texto plano que almacenan datos tabulares. Los programadores hacen esto para habilitar el intercambio de datos entre diferentes programas, sistemas, o para procesar grandes conjuntos de datos de manera eficiente y en un formato legible por humanos.

## Cómo hacerlo:
Rust, con su enfoque en seguridad y rendimiento, ofrece excelentes crates (bibliotecas) para tratar con archivos CSV, siendo `csv` el más popular. También necesitarás `serde` para serializar y deserializar datos.

Primero, añade las dependencias a tu `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Leyendo CSV

Para leer un archivo CSV, define una estructura que represente tus datos y deriva `Deserialize` de `serde`:

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
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```

Un ejemplo de salida para un CSV con información de ciudades podría ser:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Escribiendo en CSV

Para escribir en un archivo CSV, define una estructura y deriva `Serialize`:

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
            city: "Los Ángeles".into(),
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

Esto creará `output.csv` con los datos:

```csv
city,state,population
Los Ángeles,CA,3979563
Chicago,IL,2695598
```

Al aprovechar el poderoso sistema de tipos de Rust y las robustas crates del ecosistema, trabajar con datos CSV se vuelve tanto eficiente como sencillo, asegurando seguridad y rendimiento en tus tareas de procesamiento de datos.
