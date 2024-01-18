---
title:                "Scomporre una data da una stringa"
html_title:           "Rust: Scomporre una data da una stringa"
simple_title:         "Scomporre una data da una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Quando si parla di "parsing di una data da una stringa" si intende l'azione di estrarre una data composta da numeri e caratteri da una stringa di testo. I programmatori spesso devono effettuare il parsing di date dal formato stringa in modo da poterle utilizzare nei loro programmi.

## Come:
Di seguito un esempio di codice in Rust che usa la libreria Chrono per effettuare il parsing di una data da una stringa:

```Rust
use chrono::NaiveDate;

fn main() {
    let date_str = "13-05-2021";
    let parsed_date = NaiveDate::parse_from_str(date_str, "%d-%m-%Y").unwrap();
    println!("{:?}", parsed_date);
}
```
Output: 2021-05-13

## Analisi approfondita:
Il parsing di una data da una stringa è una pratica comune nei linguaggi di programmazione per la gestione delle date. In passato, questa operazione poteva essere un po' più complicata, ma grazie alla libreria Chrono in Rust, è diventata molto più semplice ed efficiente.

Esistono anche altre opzioni per il parsing delle date, come la libreria DateTime di Rust che permette di manipolare le date in modo più avanzato. Tuttavia, per semplicità ed efficienza, la libreria Chrono è spesso la scelta preferita.

## Vedi anche:
- Documentazione ufficiale di Chrono: https://docs.rs/chrono/latest/chrono/
- Esempi di utilizzo di DateTime in Rust: https://www.tutorialspoint.com/datetime-function-in-rust