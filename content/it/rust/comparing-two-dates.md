---
title:                "Confrontare due date"
html_title:           "Rust: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La comparazione di due date è un'operazione comune nella programmazione che consiste nel confrontare due date per determinare se una è precedente, uguale o successiva all'altra. I programmatori lo fanno per gestire ordini di eventi, rendere le informazioni più comprensibili e accertarsi che i dati siano accurati.

## Come fare:
```Rust
use chrono::{Datelike, Timelike};

// Definisci due date
let prima_data = NaiveDate::from_ymd(2020, 01, 01);
let seconda_data = NaiveDate::from_ymd(2021, 05, 12);

// Confronta le date
if prima_data < seconda_data {
    println!("La prima data è precedente alla seconda data.");
} else if prima_data == seconda_data {
    println!("Le due date sono uguali.");
} else {
    println!("La prima data è successiva alla seconda data.");
}

/* Output: La prima data è precedente alla seconda data. */
```

## Approfondimento:
La comparazione di date è diventata sempre più importante con l'avvento dei computer e dei sistemi informatici, che hanno la necessità di gestire e confrontare grandi quantità di informazioni. Un'alternativa alla comparazione diretta è l'utilizzo di timestamp o numeri interi che rappresentano una data in un formato standard. Nella libreria standard di Rust, è possibile utilizzare il modulo `std::time` per ottenere il timestamp di una data. Inoltre, la libreria esterna `chrono` offre una vasta gamma di opzioni per gestire e confrontare date in modo più dettagliato.

## Vedi anche:
- [La documentazione sulla libreria `chrono`](https://docs.rs/chrono/latest/chrono/)
- [La documentazione sulla libreria `time`](https://doc.rust-lang.org/std/time/)