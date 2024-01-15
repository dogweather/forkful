---
title:                "Lavorare con i file csv"
html_title:           "Rust: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo parleremo di come lavorare con i file CSV utilizzando il linguaggio di programmazione Rust. Se stai cercando un modo veloce e affidabile per gestire grandi quantità di dati in formato CSV, allora questo è l'articolo giusto per te.

## Come fare

Per utilizzare i file CSV in Rust, dovrai prima importare la libreria "csv" nel tuo progetto. Puoi farlo aggiungendo la seguente linea nel tuo file "Cargo.toml":

```rust
[dependencies]
csv = "1.0"
```

Una volta importata la libreria, puoi iniziare a utilizzarla nel tuo codice. Ad esempio, se vuoi leggere un file CSV e stampare il suo contenuto a schermo, puoi utilizzare il seguente codice:

```rust
use std::error::Error;
use std::io;
use csv::Reader;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("città.csv")?;

    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Questo codice utilizza il modulo "csv" per aprire e leggere il file "città.csv" e quindi stampa ogni singola riga del file. Il risultato dovrebbe essere simile a questo:

```rust
OK("Roma")
OK("Milano")
OK("Napoli")
```

Se invece vuoi scrivere su un file CSV, puoi utilizzare il seguente codice:

```rust
use std::error::Error;
use csv::Writer;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("nuovo_file.csv")?;

    wtr.write_record(&["Nome", "Cognome", "Età"])?;
    wtr.write_record(&["Marco", "Rossi", "30"])?;
    wtr.write_record(&["Lucia", "Bianchi", "25"])?;
    wtr.write_record(&["Giuseppe", "Verdi", "40"])?;

    wtr.flush()?;
    Ok(())
}
```

Questo codice crea un nuovo file CSV chiamato "nuovo_file.csv" e vi scrive tre righe di dati. Puoi aprire il file e verificare che i dati siano stati scritti correttamente.

## Approfondimento

Oltre a leggere e scrivere file CSV, la libreria "csv" in Rust offre molte altre funzionalità utili. Ad esempio, è possibile specificare un delimitatore diverso dal solito (virgola), gestire i valori tra virgolette o trattare le righe come strutture anziché stringhe. Per saperne di più su queste funzionalità avanzate, consulta la documentazione ufficiale della libreria "csv".

## Vedi anche

- Documentazione ufficiale della libreria "csv": https://docs.rs/csv/1.0.0/csv/
- Esempi di codice per lavorare con i file CSV in Rust: https://github.com/BurntSushi/rust-csv/tree/master/examples
- Esempio di progetto in Rust che utilizza file CSV: https://github.com/KaitouDoragon/Rust-Csv-Processor