---
title:                "Rust: Calcolo di una data nel futuro o nel passato."
simple_title:         "Calcolo di una data nel futuro o nel passato."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni di programmazione. Ad esempio, si potrebbe voler calcolare la data di scadenza di un contratto o il giorno in cui si compiono un certo numero di anni. In questo post scopriremo come farlo utilizzando il linguaggio di programmazione Rust.

## Come Fare

Per calcolare una data nel futuro o nel passato con Rust, dobbiamo prima importare la libreria `chrono`, che ci permette di manipolare le date in modo semplice e completo. Vediamo un esempio pratico:

```Rust
use chrono::{Utc, Duration};

let today = Utc::today(); // prende la data odierna
let future = today + Duration::days(14); // calcola la data di 14 giorni nel futuro
let past = today - Duration::weeks(3); // calcola la data di 3 settimane nel passato
```

In questo semplice codice, utilizziamo la struttura `Duration` per specificare quanti giorni o settimane vogliamo aggiungere o sottrarre dalla data odierna. Possiamo anche specificare altri unità di tempo come ore, minuti e secondi. Una volta calcolata la nuova data, possiamo utilizzarla come meglio preferiamo nel nostro programma.

## Approfondimento

La libreria `chrono` di Rust offre molte altre funzionalità per manipolare le date. Ad esempio, possiamo creare una data specifica utilizzando la struttura `Date` oppure ottenere l'ora corrente con la struttura `Time`. Inoltre, possiamo formattare le date con diversi stili e zone orarie. Per ulteriori informazioni, ti consiglio di leggere la documentazione ufficiale della libreria.

## Vedi Anche

- [Documentazione ufficiale di Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Corso su Rust su Udemy (in italiano)](https://www.udemy.com/course/programmare-con-rust/)
- [Tutorial per principianti su Rust (in italiano)](https://www.tutorialspoint.com/rtutorial/index.htm)