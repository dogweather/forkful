---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Confrontare due date significa verificare quale data è successiva, precedente o se sono uguali. I programmatori fanno ciò per gestire eventi, registrare il tempo, programmare attività, e molto altro.

## Come fare:

Ecco un esempio di come confrontare due date in Rust:

```Rust
use std::cmp;
use chrono::{Utc, DateTime};

pub fn compare_dates() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now();

    match date1.cmp(&date2) {
        cmp::Ordering::Less => println!("date1 è meno recente di date2"),
        cmp::Ordering::Equal => println!("date1 e date2 sono uguali"),
        cmp::Ordering::Greater => println!("date1 è più recente di date2"),
    }
}
```

Quando esegui questo codice, vedrai un output che indica se `date1` è meno recente, uguale o più recente di `date2`.

## Approfondimenti

Il confronto tra due date è un processo comune in programmazione, esistente fin dall'adozione di sistemi che utilizzano il tempo in modo significativo. In Rust, possiamo utilizzare il metodo `cmp` per fare ciò. Questo metodo restituisce un'enumerazione `Ordering` che può essere `Less` (se la prima data è meno recente), `Equal` (se sono uguali) o `Greater` (se la prima data è più recente).

Esistono alternative all'utilizzo di `cmp`, come `eq` per verificare l'uguaglianza o `lt` e `gt` per verificare se una data è meno o più recente. Tuttavia, `cmp` è spesso preferito perché fornisce tutte e tre le verifiche in una volta.

La libreria `chrono` che usiamo in questo esempio è una scelta popolare per la gestione delle date in Rust grazie alla sua robustezza e facilità d'uso. Gestisce anche correttamente i casi di orario estivo, fusi orari, ecc.

## Per approfondire

1. Documentazione ufficiale su `cmp` in Rust: https://doc.rust-lang.org/std/cmp/enum.Ordering.html
2. Documentazione ufficiale su `chrono` in Rust: https://docs.rs/chrono/0.4.19/chrono/
3. Libreria di date alternative `time`: https://docs.rs/time/0.3.2/time/