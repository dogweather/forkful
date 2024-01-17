---
title:                "Lavorare con csv"
html_title:           "Rust: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
CSV, o Comma-Separated Values, è un formato di file utilizzato per archiviare dati in una forma tabulare, in cui ogni riga rappresenta una riga della tabella e le colonne sono separate da virgole. I programmatori spesso lavorano con file CSV per importare, esportare o manipolare grandi quantità di dati in modo semplice e veloce.

## Come fare:
Per lavorare con file CSV in Rust, è possibile utilizzare la libreria [csv](https://crates.io/crates/csv). Ecco un esempio di codice che legge un file CSV e stampa il contenuto su schermo:
```Rust
use csv::Reader;

let mut reader = Reader::from_path("dati.csv").expect("Impossibile aprire il file CSV");

for result in reader.records() {
    let record = result.expect("Errore nella lettura del record");
    println!("{:?}", record);
}
```
Questo codice utilizza il metodo `from_path()` per creare un reader che legge dal file indicato. Successivamente, utilizza un ciclo for per accedere ai singoli record all'interno del file e stamparli utilizzando la funzione `println!()`. 

## Approfondimento:
Il formato CSV è stato originariamente creato per essere utilizzato con fogli elettronici come Excel, ma è diventato molto popolare anche come formato di esportazione e importazione per database e altri programmi che lavorano con dati tabellari. Esistono anche alternative come TSV, che utilizza le tabulazioni invece delle virgole come delimitatori.

La libreria [csv](https://crates.io/crates/csv) supporta anche la scrittura su file CSV e offre opzioni per personalizzare delimitatori e citazioni dei valori.

## Vedere anche:
- [Documentazione di csv](https://docs.rs/csv/)
- [Esempi di utilizzo di csv](https://github.com/BurntSushi/rust-csv/tree/master/examples)