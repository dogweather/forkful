---
title:                "Rust: Calcolare una data nel futuro o nel passato"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti aver bisogno di calcolare una data nel futuro o nel passato. Ad esempio, potresti dover programmare una task o un evento futuro, o forse stai scrivendo un'applicazione per la gestione di appuntamenti e hai bisogno di calcolare date in modo dinamico. In ogni caso, sapere come calcolare una data in Rust è un'abilità utile da avere.

## Come Fare

In Rust, esistono diverse librerie che possono aiutarti a calcolare date. In questo blog post, ci concentreremo sulla libreria Chrono, una delle più popolari librerie per la gestione del tempo in Rust.

Iniziamo importando la libreria nel nostro progetto:

```Rust
use chrono::{DateTime, Local, Duration};
```

Ora, per calcolare una data nel futuro o nel passato, dobbiamo prima definire una data iniziale. Possiamo farlo utilizzando il metodo `now()` per ottenere la data e l'ora correnti.

```Rust
let today = Local::now(); // data corrente
```

Supponiamo che vogliamo calcolare la data tra un mese. Per fare ciò, dobbiamo utilizzare il metodo `with_month()` che ci permette di specificare il numero del mese desiderato.

```Rust
let one_month_later = today.with_month(today.month() + 1).unwrap();
```

In questo esempio, stiamo calcolando la data di un mese dopo quella corrente, utilizzando il metodo `month()` per ottenere il numero del mese corrente e aggiungendo 1. È importante notare che il metodo `with_month()` restituisce un `Result`, quindi dobbiamo utilizzare il metodo `unwrap()` per ottenere la data finale.

Se vogliamo invece calcolare una data nel passato, possiamo utilizzare il metodo `with_month_past()`.

Oltre a specificare il numero del mese, possiamo anche specificare un'espressione di `Duration` per calcolare una data in base a un periodo di tempo specificato. Ad esempio, per calcolare la data tra un anno e tre mesi, possiamo utilizzare il metodo `with_year()` per impostare l'anno corrente più uno, e poi aggiungere tre mesi utilizzando il metodo `with_month()` e un espressione di `Duration` di 3 mesi.

```Rust
let one_year_three_months_later = today.with_year(today.year() + 1).unwrap().with_month(today.month() + Duration::months(3)).unwrap();
```

Ora che abbiamo una data calcolata, possiamo utilizzarla per eseguire diverse operazioni, come ad esempio stamparla a schermo, confrontarla con altre date o utilizzarla in altre funzioni.

## Deep Dive

La libreria Chrono offre molte altre funzionalità per la gestione delle date, come ad esempio il calcolo di intervalli di tempo, il parsing e la formattazione di date in vari formati, la gestione dei fusi orari e molto altro. Se sei interessato a saperne di più, puoi consultare la documentazione ufficiale della libreria [qui](https://docs.rs/chrono/) e il repository su GitHub [qui](https://github.com/chronotope/chrono).

## Vedi Anche

- [Uso delle date in Rust](https://dev.to/amalrizal13/using-dates-in-rust-f84)
- [Manipolazione del tempo in Rust con Chrono](https://dev.to/l00x/manipulating-time-in-rust-with-chrono-3p62)
- [Convertire date in diversi formati con Chrono](https://blog.logrocket.com/how-to-convert-date-to-a-different-format-in-rust/)