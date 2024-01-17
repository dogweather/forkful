---
title:                "Scrivere su standard error"
html_title:           "Rust: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cosa e perché?

Scrivere su standard error, noto anche come stderr, è un modo per i programmatori di gestire gli errori nella loro applicazione. Invece di visualizzare gli errori sullo standard output, che è dove i messaggi di output di solito appaiono, è possibile visualizzarli su standard error per una migliore gestione degli errori.

## Come fare:

Ecco un esempio di codice in Rust che mostra come scrivere su standard error utilizzando la funzione `eprintln!`:

```Rust
fn main() {
    eprintln!("Questo è un messaggio di errore!");
}
```

Ecco l'output:

```
Questo è un messaggio di errore!
```

## Approfondimento:

Scrivere su standard error è stato introdotto nei primi sistemi operativi Unix, dove era considerato una convenzione migliore rispetto alla scrittura su standard output. Questo perché gli errori devono essere gestiti in modo diverso rispetto agli output regolari e la visualizzazione su standard error aiuta i programmatori a gestirli in modo più efficace.

In alternativa, è possibile utilizzare la funzione `eprint!` per scrivere su standard error, che è simile a `print!` ma mostra il messaggio di output senza una nuova riga alla fine.

Per implementare la scrittura su standard error in Rust, il compilatore utilizza la funzione `std::io::stderr`, che restituisce un oggetto `Stderr` che rappresenta lo standard error del sistema.

## Vedi anche:

- [The Rust Standard Library](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [Standard Streams in Unix](https://www.unix.com/man-page/linux/7/stdout/)