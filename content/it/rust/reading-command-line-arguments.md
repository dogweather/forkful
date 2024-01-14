---
title:    "Rust: Lettura degli argomenti da riga di comando."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui un programma deve essere in grado di ricevere input dinamici dall'utente. I comandi da riga di comando sono uno dei modi più comuni per questo. In questo articolo, impareremo come leggere gli argomenti da riga di comando in Rust.

## Come fare

Per leggere gli argomenti da riga di comando in Rust, dobbiamo utilizzare la libreria `std::env`. Questa libreria ci permette di accedere agli argomenti passati al programma attraverso la funzione `std::env::args()`. Vediamo un esempio di come utilizzarla:

```Rust
use std::env;

fn main() {
    // Ottieni gli argomenti da riga di comando
    let args: Vec<String> = env::args().collect();
    // Stampa il primo argomento
    println!("Il primo argomento è: {}", args[1]);
}
```

Se eseguiamo questo programma da riga di comando con un argomento, ad esempio `./programma argomento`, il risultato sarà `Il primo argomento è: argomento`.

Inoltre, possiamo utilizzare il metodo `len()` sulla nostra lista di argomenti per controllare quanti argomenti sono stati passati. Possiamo anche ciclare su tutti gli argomenti utilizzando un ciclo `for`:

```Rust
// Controlla se ci sono almeno 2 argomenti presenti
if args.len() > 1 {
    // Itera su tutti gli argomenti tranne il primo (il primo è il nome del programma)
    for arg in args.iter().skip(1) {
        println!("Argomento: {}", arg);
    }
}
```

Questo è solo un esempio di come leggere gli argomenti da riga di comando in Rust, ma ci sono molte altre funzionalità della libreria `std::env` che possono essere utilizzate.

## Approfondimento

Oltre alla funzione `args()`, la libreria `std::env` ci offre anche altre utili funzioni, come ad esempio `current_dir()` per ottenere la directory corrente del programma o `var()` per accedere alle variabili d'ambiente.

Inoltre, la libreria `std::env` ci permette anche di passare argomenti al programma quando viene eseguito da un altro programma, ad esempio da un altro programma Rust o da uno script bash.

## Vedi anche

- [Documentazione ufficiale di `std::env`](https://doc.rust-lang.org/std/env)
- [Tutorial su come leggere gli argomenti da riga di comando in Rust](https://www.tutorialspoint.com/rust/rust_command_line_arguments.htm)
- [Esempi di utilizzo dei comandi da riga di comando in Rust](https://blog.knoldus.com/command-line-programming-in-rust-an-introduction/)