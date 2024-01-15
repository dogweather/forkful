---
title:                "Ottenere la data corrente"
html_title:           "Rust: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un programma, può essere utile avere accesso alla data corrente. Potresti volerla usare per mostrare all'utente quando è stato prodotto il risultato o per effettuare una determinata operazione solo in certi giorni. In questa guida, vedremo come ottenere la data corrente in Rust.

## Come fare

In Rust, per ottenere la data corrente possiamo utilizzare la libreria standard `std::time::SystemTime` e il suo metodo `now()`. Vediamo un esempio di codice che stampa la data corrente a schermo:

```rust
use std::time::SystemTime;

fn main() {
    let current_time = SystemTime::now(); // Ottiene la data corrente come istanza di SystemTime
    println!("{:?}", current_time); // Stampa a schermo la data corrente
}
```

Esempio di output:

```
2021-08-11 12:00:00.000000000 +0000
```

Potresti notare che l'output è in formato `YYYY-MM-DD HH:MM:SS`. Inoltre, noterai che ci sono degli zeri aggiunti alla fine, questi indicano le millisecondi e i microsecondi. Se desideri un formato più leggibile, puoi utilizzare la libreria `chrono` e il suo metodo `DateTime::now()`. Vediamo un esempio:

```rust
use chrono::prelude::*;

fn main() {
    let local_time = Local::now(); // Ottiene la data corrente come istanza di DateTime
    println!("{}", local_time.format("%Y-%m-%d %H:%M:%S")); // Stampa a schermo la data con formato personalizzato
}
```

Esempio di output:

```
2021-08-11 12:00:00
```

Puoi scegliere il formato che preferisci, basta modificare il parametro del metodo `format()` con le corrispondenti lettere dalla documentazione ufficiale di `chrono`.

## Approfondimento

La libreria `std::time::SystemTime` rappresenta il tempo dal 1 gennaio 1970 (detto anche "epoch") in secondo Unix. Ciò permette di avere una rappresentazione del tempo universale in maniera uniforme e senza bisogno di conversioni. Invece, `chrono` offre una rappresentazione più flessibile e leggibile, grazie alla sua gestione dei fusi orari. L'output della data corrente utilizzando `SystemTime` è del tipo `SystemTime` mentre utilizzando `chrono` è del tipo `DateTime`.

## Vedi anche

- Documentazione ufficiale di `std::time::SystemTime`: https://doc.rust-lang.org/std/time/struct.SystemTime.html
- Documentazione ufficiale di `chrono`: https://docs.rs/chrono/0.4.19/chrono/index.html