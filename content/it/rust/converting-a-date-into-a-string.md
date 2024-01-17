---
title:                "Convertire una data in una stringa"
html_title:           "Rust: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Convertire una data in una stringa significa semplicemente rappresentare una data in formato testuale anziché numerico. I programmatori spesso devono farlo per rendere più leggibile e comprensibile una data all'utente finale.

## Come fare:
```Rust 
use chrono::{Datelike, Timelike, NaiveDate};

// Ottenere la data corrente
let now = Local::now();

// Convertire la data in una stringa nel formato "dd/mm/yyyy"
let date_str = format!("{}/{}/{}", now.day(), now.month(), now.year());

// Convertire la data in una stringa nel formato "mm/dd/yyyy"
let date_str = format!("{}/{}/{}", now.month(), now.day(), now.year());  
```
**Output:** Nel formato "dd/mm/yyyy" o "mm/dd/yyyy" a seconda del formato scelto.

## Approfondimento:
- In passato, per rappresentare una data venivano utilizzati solamente numeri interi, come ad esempio 08/03/2021. Tuttavia, questa forma può portare a confusione tra diversi paesi che utilizzano formati di date differenti. Per questo motivo, è diventato più comune rappresentare le date in formato testuale, come 8 marzo 2021.
- Alcune alternative per convertire una data in stringa possono includere l'utilizzo di librerie esterne come "strftime" o "datetime", ma questi metodi possono essere meno efficienti e avere una sintassi più complessa rispetto alle funzioni native di Rust.
- L'implementazione di Rust si basa sulle librerie esterne "chrono" e "Local", che consentono di ottenere la data corrente e di convertirla in una stringa utilizzando metodi semplici e diretti.

## Vedi anche:
- Documentazione ufficiale di Rust per la manipolazione delle date: https://doc.rust-lang.org/std/datetime/struct.DateTime.html
- Libreria "chrono" per manipolare le date in Rust: https://docs.rs/chrono/0.4.19/chrono/