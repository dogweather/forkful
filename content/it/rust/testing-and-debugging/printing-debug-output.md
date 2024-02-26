---
date: 2024-01-20 17:53:09.331161-07:00
description: "Il debug consiste nel stampare informazioni di output per controllare\
  \ lo stato del tuo programma. Programmatori fanno ci\xF2 per diagnosticare e sistemare\
  \ i\u2026"
lastmod: '2024-02-25T18:49:41.095931-07:00'
model: gpt-4-1106-preview
summary: "Il debug consiste nel stampare informazioni di output per controllare lo\
  \ stato del tuo programma. Programmatori fanno ci\xF2 per diagnosticare e sistemare\
  \ i\u2026"
title: Stampa dell'output di debug
---

{{< edit_this_page >}}

## What & Why?
Il debug consiste nel stampare informazioni di output per controllare lo stato del tuo programma. Programmatori fanno ciò per diagnosticare e sistemare i difetti nel codice.

## How to:
Per stampare output di debug in Rust si usa la macro `println!` per testo normale, o `dbg!` per output che include il file e la linea di codice.

```Rust
fn main() {
    let numero = 42;
    println!("Il numero è: {}", numero);      // Stampa semplice
    dbg!(numero);                             // Stampa di debug
}
```
Output di `println!`:
```
Il numero è: 42
```
Output di `dbg!`:
```
[src/main.rs:4] numero = 42
```

## Deep Dive
Il debug non è un'idea nuova, esiste da quando programmiamo. In Rust, `println!` è semplice e diretto, usato comunemente per vedere il flusso di esecuzione. `dbg!`, introdotto in Rust 1.32, è più potente: stampa il valore, il file, e il numero di linea, il che è utile per tracciare il flusso di dati. Un'alternativa è l'uso del logger, ma per scopi di debug rapido, `dbg!` è spesso sufficiente e veloce.

## See Also
Per imparare di più:

- [Rust std::fmt](https://doc.rust-lang.org/std/fmt/) per formattazione custom.
- [Rust dbg! macro](https://doc.rust-lang.org/std/macro.dbg.html) per dettagli sull'utilizzo della macro.
- [The Rust Programming Language](https://doc.rust-lang.org/book/) guida ufficiale del linguaggio Rust.
