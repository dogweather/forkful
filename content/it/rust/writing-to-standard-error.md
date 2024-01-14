---
title:    "Rust: Scrivere su standard error"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché Scrivere a Standard Error in Rust

Scrivere a standard error è un processo importante nel linguaggio di programmazione Rust. Spesso gli sviluppatori si trovano nella situazione in cui devono gestire output particolari o errori durante l'esecuzione del loro codice. Scrivere a standard error può aiutare a gestire questi casi in modo efficace e chiaro.

## Come Fare

Per scrivere a standard error in Rust, si può utilizzare il modulo `std::io` e la funzione`eprintln!()`:

````Rust
use std::io;

fn main() {
    let name = "Mario";
    let age = 30;
    eprintln!("Ciao, mi chiamo {} e ho {} anni.", name, age);
}
````

Output:

```
Ciao, mi chiamo Mario e ho 30 anni.
```

Possiamo anche utilizzare la macro `format!()` per creare una stringa con il messaggio di errore e passarla alla funzione `eprintln!()`:

````Rust
use std::io;

fn main() {
    let x = 10;
    let y = 0;
    if y == 0 {
        let msg = format!("Errore: tentativo di dividere {} per 0.", x);
        eprintln!("{}", msg);
    }
}
````

Output:

```
Errore: tentativo di dividere 10 per 0.
```

## Approfondimento

Scrivere a standard error è uno dei modi per gestire gli errori in Rust. Questo rende più semplice e chiaro rilevare e gestire situazioni di errore durante l'esecuzione del codice. Inoltre, il linguaggio Rust ha un'ottima gestione degli errori incorporata, permettendo agli sviluppatori di personalizzare e gestire i messaggi di errore in modo efficace e strutturato.

## Vedi Anche

- [Documentazione su std::io](https://doc.rust-lang.org/std/io/index.html)
- [Gestione degli errori in Rust](https://doc.rust-lang.org/book/ch09-03-to-panic-or-not-to-panic.html#propagating-errors)
- [Rust by Example: std::io](https://doc.rust-lang.org/stable/rust-by-example/std_misc/io.html)