---
title:                "Lettura di un file di testo"
html_title:           "Rust: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Probabilmente stai pensando di leggere un file di testo perché stai lavorando su un progetto di programmazione che richiede l'accesso a dati da un file esterno. La lettura di file di testo è un'operazione comune e fondamentale nella programmazione moderna.

## Come fare

Per leggere un file di testo utilizzando Rust, è necessario prima creare un oggetto `File` che rappresenti il file che si vuole leggere. Ciò può essere fatto utilizzando il metodo `File::open()`, fornendo il percorso del file come argomento. Ecco un esempio di codice che legge un file di testo chiamato "test.txt" e ne stampa il contenuto sulla console:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let file = File::open("test.txt").expect("Errore nell'apertura del file.");
    let mut contenuto = String::new();
    file.read_to_string(&mut contenuto).expect("Errore nella lettura del file.");
    println!("{}", contenuto);
}
```

L'output di questo codice dovrebbe essere il contenuto del file "test.txt" stampato sulla console.

## Approfondimento

La lettura dei file di testo è una delle operazioni più comuni nella programmazione e Rust offre diverse opzioni per gestirla in modo efficiente. Ad esempio, è possibile specificare l'encoding del file aperto utilizzando il metodo `File::open()`, che ha un argomento opzionale per specificarlo.

Inoltre, è possibile utilizzare il pacchetto `std::fs` per accedere a funzioni più avanzate di gestione dei file, come la lettura e la scrittura di singole linee o il controllo dei permessi di accesso.

## Vedi anche

- [Documentazione ufficiale di Rust su lettura e scrittura di file](https://doc.rust-lang.org/stable/std/fs/index.html)
- [Esempi pratici di lettura e scrittura di file con Rust](https://www.rust-lang.org/learn/get-started)
- [Tutorial su input/output in Rust](https://www.tutorialspoint.com/rust/rust_input_output.htm)