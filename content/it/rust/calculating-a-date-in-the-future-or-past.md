---
title:                "Calcolare una data nel futuro o nel passato."
html_title:           "Rust: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in numerose situazioni, come ad esempio per la gestione di eventi o per la generazione di report.

## Come fare

Per calcolare una data nel futuro o nel passato in Rust, è possibile utilizzare il modulo `chrono`, che offre diverse funzioni e strutture dati per la gestione del tempo e delle date.

Per prima cosa, è necessario importare il modulo `chrono` nel proprio codice:

```Rust
use chrono::{DateTime, Datelike, Duration, Local};
```

Per calcolare una data nel futuro, si può utilizzare la funzione `+` per aggiungere una determinata durata ad una data esistente. Ad esempio, per calcolare la data di domani:

```Rust
let today = Local::today();
let tomorrow = today + Duration::days(1);
println!("Domani sarà il {}", tomorrow);
```

In questo esempio, abbiamo utilizzato la funzione `Local::today()` per ottenere la data di oggi, e la funzione `Duration::days()` per creare una durata di un giorno.

Per calcolare una data nel passato, si può invece utilizzare la funzione `-` per sottrarre una durata ad una data esistente. Ad esempio, per calcolare la data di ieri:

```Rust
let today = Local::today();
let yesterday = today - Duration::days(1);
println!("Ieri è stato il {}", yesterday);
```

Esistono anche altre funzioni utili per gestire le date, come ad esempio `DateTime::parse_from_str()` per convertire una stringa in una data, o `DateTime::format()` per formattare una data in una stringa secondo uno specifico formato.

## Approfondimenti

Calcolare una data nel futuro o nel passato può essere più complesso di quanto sembra, poiché bisogna considerare eventi come l'ora legale, fusi orari e i giorni bisestili. A tal proposito, il modulo `chrono` offre diverse strutture dati, come `TimeZone` e `NaiveDate`, per gestire in modo accurato le date e i tempi.

Per ulteriori informazioni sul modulo `chrono` e sulle sue funzionalità, si consiglia di consultare la documentazione ufficiale su [https://docs.rs/chrono](https://docs.rs/chrono) o di visitare la pagina GitHub del progetto su [https://github.com/chronotope/chrono](https://github.com/chronotope/chrono).

## Vedi anche

- [Gestione delle date e del tempo in Rust](https://www.rust-lang.org/it/learn/dates-and-times)
- [Tutorial su chrono](https://stevedonovan.github.io/rust-gentle-intro/6-dates-and-times.html)
- [Timestamp e gestione della data in Rust](https://hackernoon.com/rust-exploring-timestamps-and-manipulating-dates-35c9b969c248)