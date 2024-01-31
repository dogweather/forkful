---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, c'est quoi ? Des fichiers avec des données séparées par des virgules. Pourquoi on l'utilise ? Simple et universel pour stocker et échanger des données tabulaires. 

## How to:

```Rust
use csv;
use std::error::Error;
use std::io;
use std::process;

fn main() {
    if let Err(err) = read_csv() {
        println!("Erreur lors de la lecture du CSV: {}", err);
        process::exit(1);
    }
}

fn read_csv() -> Result<(), Box<dyn Error>> {
    let mut rdr = csv::Reader::from_reader(io::stdin());
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn write_csv() -> Result<(), Box<dyn Error>> {
    let mut wtr = csv::Writer::from_writer(io::stdout());
    wtr.write_record(&["ville", "population", "province"])?;
    wtr.write_record(&["Paris", "2,148,271", "Île-de-France"])?;
    wtr.flush()?;
    Ok(())
}
```

```Shell
echo "ville,population,province\nParis,2148271,Île-de-France" | cargo run
["ville", "population", "province"]
["Paris", "2148271", "Île-de-France"]
```

## Deep Dive

Les CSV existent depuis les années 1970, simples mais limités. Alternatives ? JSON, XML, mais plus compliqués. Pour Rust, la bibliothèque `csv` gère bien le parsing, l'écriture et même les données non-UTF-8. Ça utilise `serde` pour la sérialisation/désérialisation, optimisant les performances et la flexibilité.

## See Also

- Documentation officielle de `csv`: [https://docs.rs/csv](https://docs.rs/csv)
- Serde, pour la sérialisation/désérialisation dans Rust: [https://serde.rs/](https://serde.rs/)
- Un how-to pour manipuler des structures de données CSV: [https://docs.rs/csv/latest/csv/tutorial/index.html](https://docs.rs/csv/latest/csv/tutorial/index.html)
