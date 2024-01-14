---
title:                "Rust: Ottenere la data corrente"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si chiedono come ottenere la data corrente nel loro codice. In questo articolo, impareremo come farlo utilizzando il linguaggio di programmazione Rust.

## Come fare

Per ottenere la data corrente in Rust, dobbiamo utilizzare il modulo `std::time` e la sua funzione `now`, che restituisce un tipo di dato `SystemTime`. Possiamo quindi utilizzare il metodo `format` per formattare la data come vogliamo.

Ecco un esempio di codice che stampa la data corrente nel formato `GG/MM/YYYY`:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::Duration;
use std::fmt::Write;

let start = SystemTime::now();
let since_the_epoch = start.duration_since(UNIX_EPOCH).expect("Time went backwards");

let mut output = String::new();
write!(output, "{}/{}/{}", since_the_epoch.day(), since_the_epoch.month(), since_the_epoch.year()).unwrap();

println!("{}", output); // output: GG/MM/YYYY
```

## Approfondimento

Per ottenere la data corrente, Rust utilizza il concetto di "epoca Unix", che rappresenta il numero di secondi passati dal 1 gennaio 1970. Utilizzando la funzione `now`, otteniamo il numero di secondi trascorsi dall'epoca fino al momento in cui viene eseguita la nostra applicazione. Successivamente, possiamo utilizzare i metodi forniti dal tipo `SystemTime` per formattare la data secondo le nostre esigenze.

## Guarda anche

- [La documentazione ufficiale di Rust su `std::time`](https://doc.rust-lang.org/std/time/index.html)
- [Un articolo sul concetto di "epoca Unix"](https://www.geeksforgeeks.org/epoch-unix-time/)