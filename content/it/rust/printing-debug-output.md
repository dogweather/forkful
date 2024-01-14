---
title:                "Rust: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'importante pratica di programmazione in Rust. Ci permette di comprendere meglio cosa sta succedendo all'interno del nostro codice e, in caso di errori, ci aiuta a identificare il problema. Inoltre, l'output di debug è uno strumento utile per verificare che il nostro codice sia eseguito correttamente.

## Come Fare

Per stampare l'output di debug in Rust, possiamo utilizzare la macro `println!()`. Questa macro prende come parametro una stringa formattata che contiene i nostri dati di debug e li stampa sulla console. Possiamo anche utilizzare la macro `dbg!()` per stampare immediatamente il valore di una variabile senza doverla convertire in una stringa.

Un esempio di codice per stampare una variabile `i` con la macro `println!()`:

```Rust
println!("Il valore di i è {}", i);
```

Un esempio di codice per stampare immediatamente il valore di una variabile `j` con la macro `dbg!()`:

```Rust
dbg!(j);
```

L'output di questi due esempi potrebbe essere qualcosa del genere:

```
Il valore di i è 5
[j: 10]
```

## Approfondimento

Oltre alla macro `println!()` e `dbg!()`, Rust ci offre anche la possibilità di stampare l'output di debug con la macro `eprintln!()`. Questa macro stampa l'output su `stderr` invece che su `stdout`. Inoltre, possiamo utilizzare il modificatore `?` dopo una variabile, chiamato `Debug trait`, per stampare automaticamente l'output di debug senza dover utilizzare esplicitamente le macro.

Un esempio di utilizzo del modificatore `?` per stampare l'output di debug di una variabile `k`:

```Rust
println!("La variabile k è {:?}", k);
```

L'output di questo esempio potrebbe essere qualcosa come:

```
La variabile k è 12.5
```

## Vedi Anche

- [La documentazione ufficiale di Rust sull'output di debug](https://doc.rust-lang.org/std/fmt/index.html)
- [Un articolo su Medium che spiega come utilizzare l'output di debug in Rust](https://medium.com/@KodSquad/debugging-in-rust-70db11e8bdb3)
- [Un thread di Reddit su diverse tecniche per l'output di debug in Rust](https://www.reddit.com/r/rust/comments/fn0atd/rust_debugging_techniques_that_helped_me/)