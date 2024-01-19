---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La lettura di un file di testo è la capacità di un programma di accedere ai dati memorizzati in questo tipo di file. I programmatori lo fanno per gestire i dati, per analizzarli, per memorizzare le configurazioni dell'applicazione, e in molti altri casi.

## Come fare:

Per leggere un file di testo in Rust, utilizziamo il modulo `std::fs::File` e `std::io::Read`. Ecco un esempio molto semplice:

```Rust
use std::fs::File;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let mut file = File::open("testo.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("{}", contents);
    Ok(())
}
```
In questo esempio, apriamo un file chiamato "testo.txt", leggiamo tutto il suo contenuto in una stringa, e lo stampiamo sul terminale.

## Approfondimento

Dal punto di vista storico, Rust ha sempre avuto un forte focus sulla sicurezza, e questo si riflette nella lettura dei file. Le operazioni IO potrebbero fallire in molti modi, quindi è fondamentale la gestione degli errori, come dimostra l'esempio di cui sopra.

C'è anche un'altra opzione per leggere un file di testo in Rust che è il modulo `std::fs::read_to_string`:

```Rust
use std::fs;

fn main() -> std::io::Result<()> {
    let contents = fs::read_to_string("testo.txt")?;
    println!("{}", contents);
    Ok(())
}
```
Questo esempio fa essenzialmente la stessa cosa del primo, ma con meno codice. È una funzione di comodo che nasconde alcuni dettagli di implementazione.

## Vedi anche:

Per ulteriori informazioni sulla gestione dei file in Rust, consulta queste risorse:

- [The Rust Programming Language Book: File I/O](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)
- [Rust by Example: File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html)
- [The `std::fs` module documentation](https://doc.rust-lang.org/std/fs/index.html)