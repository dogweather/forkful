---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Tratteggiando il Tempo: Ottenere la Data Corrente in Rust

## Cosa e Perché?
Ottenere la data corrente significa accedere all'istante attuale del sistema. I programmatori lo utilizzano spesso per tracciare eventi, generare timestamp o gestire funzionalità basate sul tempo.

## Come si fa:
Per ottenere la data corrente in Rust, utilizziamo il modulo `chrono`. Ecco come:

```Rust
// Importiamo il modulo chrono
extern crate chrono;
use chrono::prelude::*;

fn main() {
    // Ottenere la data e l'ora attuale
    let now = Local::now();

    println!("{}", now);
}
```
L'output sarà la data e l'ora corrente nella tua zona oraria.

## Approfondimenti
La gestione del tempo non è un concetto nuovo nella programmazione. Infatti, viene utilizzata sin dai primi giorni dell'informatica. In Rust, `chrono` è il modulo principale per il trattamento del tempo. 

Esistono alternative a `chrono`, come `time` e `naive-date`, ma `chrono` è più completo e semplice da utilizzare.

Ottenere la data corrente in `chrono` avviene tramite il metodo `now` della struttura `Local`. `Local` rappresenta il fuso orario locale del sistema.

## Per Saperne di Più
Per ulteriori dettagli su come gestire il tempo e la data in Rust, consulta la documentazione ufficiale di `chrono`:
[chrono - Rust](https://docs.rs/chrono/0.4.19/chrono/)