---
title:                "Lavorare con json"
html_title:           "Rust: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON? 
Innanzitutto, JSON è un formato di dati molto popolare e ampiamente utilizzato per la comunicazione tra applicazioni web. Inoltre, poiché Rust ha un sistema di tipizzazione statica e un'eccellente gestione della memoria, è ideale per manipolare e analizzare dati JSON in modo efficiente.

## Come fare 
Per lavorare con JSON in Rust, è necessario importare la libreria `serde` e la libreria `serde_json` nel proprio progetto. Ecco un esempio di codice che legge un file JSON e stampa il contenuto su console:

```rust
extern crate serde;
extern crate serde_json;

use std::fs::File;
use std::io::prelude::*;
use serde_json::Value;

fn main() {
    // Apri il file JSON
    let mut file = File::open("data.json").expect("Impossibile aprire il file");

    // Leggi il contenuto del file in una stringa
    let mut content = String::new();
    file.read_to_string(&mut content).expect("Impossibile leggere i dati");

    // Parsing del contenuto in un oggetto Value
    let data: Value = serde_json::from_str(&content).expect("Impossibile convertire in JSON");

    // Stampa il contenuto su console
    println!("{}", data);
}
```

Output:
```bash
{"nome": "Mario", "eta": 30, "hobby": ["calcio", "leggere", "giardinaggio"]}
```

## Deep Dive 
La libreria `serde` fornisce una serie di annotazioni che permettono di specificare come un tipo di dato deve essere serializzato o deserializzato in formato JSON. Ad esempio, l'annotazione `#[derive(Serialize, Deserialize)]` può essere utilizzata per indicare che una struttura o una enumerazione deve essere serializzata o deserializzata in JSON. Inoltre, è possibile utilizzare `serde_json::to_string()` e `serde_json::from_str()` per convertire manualmente i dati in formato JSON.

## Vedi anche 
- [Documentazione di `serde`](https://serde.rs/)
- [Documentazione di `serde_json`](https://docs.serde.rs/serde_json/)