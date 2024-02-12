---
title:                "Trabalhando com CSV"
aliases:
- /pt/rust/working-with-csv.md
date:                  2024-02-03T19:21:24.536383-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com arquivos CSV (Valores Separados por Vírgula) é sobre a leitura e escrita de arquivos de texto simples que armazenam dados tabulares. Programadores fazem isso para permitir o compartilhamento de dados entre diferentes programas, sistemas, ou para processar conjuntos de dados grandes de forma eficiente e legível para humanos.

## Como fazer:
Rust, com seu foco em segurança e desempenho, oferece excelentes crates (bibliotecas) para lidar com arquivos CSV, sendo `csv` a mais popular. Você também precisará do `serde` para serializar e desserializar dados.

Primeiro, adicione as dependências ao seu `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Lendo CSV

Para ler um arquivo CSV, defina uma struct que represente seus dados e derive `Deserialize` de `serde`:

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

A saída de amostra para um CSV com informações da cidade pode parecer:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Escrevendo em CSV

Para escrever em um arquivo CSV, defina uma struct e derive `Serialize`:

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

Isso criará `output.csv` com os dados:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Ao aproveitar o poderoso sistema de tipos do Rust e os robustos crates do ecossistema, trabalhar com dados CSV torna-se eficiente e simples, garantindo segurança e desempenho em suas tarefas de processamento de dados.
