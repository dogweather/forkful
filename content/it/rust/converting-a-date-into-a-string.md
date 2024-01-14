---
title:                "Rust: Convertire una data in una stringa"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai scrivendo un'applicazione in Rust che richiede la manipolazione delle date, potresti trovarti nella situazione in cui devi convertire una data in una stringa. Ci sono varie ragioni per cui potresti avere bisogno di fare ciò, ad esempio per mostrare una data all'utente o per salvare una data in un database.

## Come fare

Per convertire una data in una stringa in Rust, è possibile utilizzare il tipo di dato "NaiveDate" della libreria "chrono". Questo tipo di dato permette di rappresentare una data senza il fuso orario.

```Rust
use chrono::{NaiveDate};

fn main() {
    let date = NaiveDate::from_ymd(2021, 06, 20); // Creazione di una data

    let stringa_data = date.to_string(); // Conversione in stringa

    println!("{}", stringa_data); // Stampa della data come stringa
}
```

Output: `2021-06-20`

## Approfondimento

La libreria "chrono" offre anche la possibilità di formattare la data in modo personalizzato, utilizzando il metodo "format". Questo metodo restituisce un oggetto "DateTimeFormatter" che può essere utilizzato per specificare il formato della data.

```Rust
use chrono::{NaiveDate, DateTime, Local, Datelike, Timelike};

fn main() {
    let date = NaiveDate::from_ymd(2021, 06, 20); // Creazione di una data

    let datetime = DateTime::from_utc(date.and_hms(10, 30, 0), Local); // Creazione di un DateTime con orario locale

    let formatter = datetime.format("%d/%m/%Y - %H:%M"); // Specifica del formato della data

    let formatted = formatter.to_string(); // Conversione in stringa

    println!("{}", formatted); // Stampa della data formattata
}
```

Output: `20/06/2021 - 10:30`

## Vedi anche

- [Documentazione ufficiale di Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Tutorial su come formattare le date in Rust](https://www.techiediaries.com/rust-dates-format-strftime/)