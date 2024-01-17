---
title:                "Leggere un file di testo"
html_title:           "Rust: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La lettura di un file di testo è un'operazione comune per i programmatori, che consiste nell'accedere ai contenuti di un file di testo attraverso il codice. Questo può essere utile per leggere dati da un file di configurazione, un file di log o per elaborare informazioni strutturate contenute in un testo.

## Come fare:
```Rust
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

fn main() {
    let file = File::open("test.txt").expect("Impossibile aprire il file");
    let reader = BufReader::new(file);
    for line in reader.lines() {
        println!("{}", line.unwrap());
    }
}
```
Esempio di output:
```
Ciao, questo è un file di testo
utilizzato per mostrare come leggere un file in Rust.
```

## Deep Dive:
La lettura di file di testo è supportata nativamente in Rust attraverso il modulo `std::fs` che offre funzionalità per accedere ai file e creare lettore di buffer per la loro lettura. In passato, i programmatori utilizzavano librerie esterne come `std::fs::File` o `std::fs::CanRead` per leggere i file di testo. Tuttavia, a partire dalla versione 1.16 di Rust, è stata introdotta l'implementazione dei lettori di buffer in modo nativo per migliorare le prestazioni e semplificare il processo di lettura dei file.

## Vedi anche:
- [Documentazione del modulo std::fs in Rust](https://doc.rust-lang.org/std/fs/)
- [Rust Cookbook: Lettura di un file](https://rust-lang-nursery.github.io/rust-cookbook/file/internals.html)