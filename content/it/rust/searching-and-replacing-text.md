---
title:    "Rust: Cercare e sostituire il testo"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono operazioni comuni durante la scrittura del codice. Con l'aiuto di Rust, possiamo automatizzare questo processo per risparmiare tempo e ridurre gli errori umani.

## Come fare

Per iniziare, dobbiamo importare il modulo `std::fs` per leggere e scrivere file e il modulo `std::io::{BufRead, BufReader, BufWriter, Write}` per gestire il buffering dei file in modo efficace.

```Rust
use std::fs;
use std::io::{BufRead, BufReader, BufWriter, Write};
```

Creiamo un nuovo file di input `input.txt` con il seguente contenuto:

> Questo è un esempio di testo che vogliamo modificare.

Ora possiamo creare un nuovo file `output.txt` e copiare il contenuto di `input.txt` sostituendo le parole "esempio" con "prova".

```Rust
let input_file = fs::File::open("input.txt").expect("Impossibile aprire il file di input.");
let output_file = fs::File::create("output.txt").expect("Impossibile creare il file di output.");

let reader = BufReader::new(input_file);
let mut writer = BufWriter::new(output_file);

for line in reader.lines() {
    let line = line.expect("Impossibile leggere la linea.");
    let new_line = line.replace("esempio", "prova");
    writeln!(writer, "{}", new_line).expect("Impossibile scrivere il file.");
}

writer.flush().expect("Impossibile aggiornare il file.");


```

L'output nel file `output.txt` sarà:

> Questo è un prova di testo che vogliamo modificare.

## Approfondimento

Esistono diverse funzioni in Rust per la ricerca e sostituzione di testo, come `replace()`, `replacen()` e `replacen_ignore_ascii_case()`. Oltre a ciò, possiamo utilizzare librerie di espressi regolari come `regex` per rendere le nostre sostituzioni ancora più flessibili.

## Vedi anche

- [Documentazione di Rust per il modulo fs](https://doc.rust-lang.org/std/fs/index.html)
- [Documentazione di Rust per il modulo io](https://doc.rust-lang.org/std/io/)
- [Libreria regex per Rust](https://crates.io/crates/regex)