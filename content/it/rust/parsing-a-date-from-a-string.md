---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il parsing di una data da una stringa significa prendere un testo (spesso in un formato predefinito) e trasformarlo in un oggetto data. I programmatori lo fanno per leggere le date nei formati più vari e utilizzarle nel proprio codice.

## Come fare:

Ecco un esempio su come fare il parsing di una data da una stringa con Rust:

```Rust
use chrono::{NaiveDate, ParseError};

fn string_to_date(date_str: &str) -> Result<NaiveDate, ParseError> {
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
}

fn main() {
    let data_testo = "2022-06-15";

    match string_to_date(data_testo) {
        Ok(data) => println!("La data è: {}", data),
        Err(e) => println!("Errore nel parsing della data: {:?}", e),
    }
}
```

L'output sarà:
`La data è: 2022-06-15`

## Approfondimento:

Per comprendere meglio, parliamo un po' del contesto storico, delle alternative e alcuni dettagli di implementazione riguardo al parsing di una data da una stringa.

(1) Contesto Storico: Originariamente, date e orari in Rust venivano gestiti come interi, ma poi è stata introdotta la libreria Chrono per facilitare questo processo.

(2) Alternative: Oltre a Chrono, ci sono altre librerie come "time" e "date-time-format”, che offrono anche la possibilità di gestire le date.

(3) Dettagli di Implementazione: date e ore in Rust vengono frequentemente rappresentate come strutture NaiveDate e NaiveTime. La funzione parse_from_str viene usata per fare il parsing di una data da una stringa.

## Vedi anche: 

Per ulteriori informazioni, consulta i seguenti link:

1. [Documentazione ufficiale Rust](https://doc.rust-lang.org/book/)
2. [Documentazione libreria Chrono](https://docs.rs/chrono/0.4.19/chrono/)
3. [Discussione StackOverflow sul parsing di date in Rust](https://stackoverflow.com/questions/41474224/how-to-parse-a-date-string-in-the-format-yyyy-mm-dd-in-rust)