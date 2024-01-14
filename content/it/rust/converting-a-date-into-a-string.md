---
title:                "Rust: Convertingire una data in una stringa"
simple_title:         "Convertingire una data in una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Sempre più spesso, quando si lavora con dati e informazioni, si ha bisogno di rappresentare una data in formato testuale. Questo può essere utile per visualizzare le informazioni in un formato più comprensibile per gli utenti o per eseguire operazioni di confronto. In questa guida impareremo come convertire una data in una stringa utilizzando il linguaggio di programmazione Rust.

## Come fare

Per convertire una data in una stringa in Rust, ci sono diverse opzioni disponibili. Un modo è utilizzare la libreria standard di Rust, che fornisce la funzione `to_string()` sulla struttura `Date` per convertirla in una stringa. Ecco un esempio di codice che mostra come utilizzare questa funzione:

```Rust
use chrono::{Date, Utc};
use std::string::ToString;

fn main() {
    let date = Utc::today();
    let date_string = date.to_string();
    println!("{}", date_string); // Output: 2021-09-24
}
```

Puoi anche personalizzare il formato della stringa di output utilizzando la funzione `format()` sulla struttura `Date`. Ecco un esempio di codice che mostra come fare:

```Rust
use chrono::{Date, Utc};
use std::string::ToString;

fn main() {
    let date = Utc::today();
    let date_string = date.format("%d/%m/%Y").to_string();
    println!("{}", date_string); // Output: 24/09/2021
}
```

## Approfondimento

Oltre alla libreria standard di Rust, ci sono anche diverse librerie di terze parti che forniscono funzionalità per convertire una data in una stringa. Ad esempio, la libreria `time` consente di formattare una data utilizzando il linguaggio di formattazione POSIX. Ecco un esempio di codice che utilizza questa libreria:

```Rust
use time::{OffsetDateTime, PrimitiveDateTime};

fn main() {
    let datetime = PrimitiveDateTime::new(2021, 9, 24, 0, 0, 0);
    let offset_datetime = OffsetDateTime::from_datetime(datetime, 0);
    println!("{}", offset_datetime.as_strftime("%d/%m/%Y").unwrap()); // Output: 24/09/2021
}
```

Inoltre, è possibile modificare le impostazioni di localizzazione per ottenere una stringa di data formattata in un formato specifico della lingua e della regione. Ciò può essere fatto utilizzando la libreria `chrono-locale`. Ecco un esempio di codice che mostra come utilizzare questa libreria:

```Rust
use chrono::{Date, DateTime, Utc};
use chrono_locale::LocaleDate;
use std::string::ToString;

fn main() {
    let date: Date<Utc> = Utc::today();
    let locale = LocaleDate::with_time(date, "%d/%m/%Y", "it-IT");
    let date_string = locale.ymd();
    println!("{}", date_string); // Output: 24/09/2021
}
```

## Vedi anche

- Documentazione ufficiale: https://doc.rust-lang.org/std/string/trait.ToString.html
- Libreria `time`: https://docs.rs/time/0.3.2/time/
- Libreria `chrono-locale`: https://docs.rs/chrono-locale/0.1.1/chrono_locale/struct.LocaleDate.html