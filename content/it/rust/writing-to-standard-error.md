---
title:                "Rust: Scrivere su errore standard"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su errori standard (*standard error*) è un'importante parte della programmazione Rust. Questo tipo di output è utile per il debugging e la gestione degli errori nel tuo codice. Scopriamo come farlo!

## Come Fare

Per scrivere su errori standard in Rust, è necessario utilizzare il modulo `std::io`, che contiene il trait `Write` che fornisce il metodo `write_all` per scrivere su un buffer. I passaggi da seguire sono i seguenti:

1. Importa il modulo `std::io` utilizzando `use std::io::Write;`.
2. Crea un oggetto `std::io::stderr` che rappresenta l'output di errori standard.
3. Utilizza il metodo `write_all` per scrivere sulla standard error, fornendo come argomento una slice di byte del messaggio che vuoi scrivere.
4. Usa il metodo `flush` per assicurarti che il messaggio venga scritto correttamente.

Ecco un esempio di codice che scrive "Hello World" sulla standard error:

```Rust
use std::io::Write;

fn main() {
    let mut stderr = std::io::stderr();
    stderr.write_all(b"Hello World").expect("Errore durante la scrittura");
    stderr.flush().expect("Errore durante il flushing");
}
```

La slice di byte `b"Hello World"` è l'equivalente del messaggio di testo in formato binario. È importante notare che il metodo `write_all` restituisce un `Result` che può essere gestito in caso di errori.

## Deep Dive

Quando si scrive su errori standard in Rust, è importante capire che questo tipo di output viene mostrato sul terminale solo in caso di errori nel programma. In caso contrario, non vedremo nulla. Ciò è dovuto al fatto che la standard error è diversa dalla standard output, che viene utilizzata per mostrare i risultati dei nostri programmi.

Inoltre, possiamo utilizzare la macro `eprintln!` per semplificare il codice di scrittura su errori standard. Questa macro funziona come la più comune `println!`, ma scrive sulla standard error invece che sulla standard output.

## See Also
- [Rust book - Writing to standard error](https://doc.rust-lang.org/book/ch07-04-bringing-paths-into-scope-with-the-use-keyword.html)
- [Writing to stderr in Rust](https://rust-lang-nursery.github.io/rust-cookbook/os/stdio.html#writing-to-stderr)

Scrivere su errori standard può sembrare complicato, ma con pochi semplici passaggi possiamo farlo facilmente in Rust. Spero che questo articolo sia stato utile per comprendere meglio questo aspetto della programmazione in Rust. Continuate a esplorare e a imparare sempre di più sulla lingua e sulle sue funzionalità!