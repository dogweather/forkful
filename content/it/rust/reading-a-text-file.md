---
title:                "Rust: Leggere un file di testo"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Probabilmente ti sarai chiesto perché dovresti leggere un file di testo in Rust. La risposta è semplice: la programmazione in Rust è molto semplice e rapida, e leggere un file di testo non fa eccezione. Inoltre, imparare a leggere e manipolare i dati da un file di testo può essere un'abilità molto utile in molti scenari diversi.

## Come fare
Per iniziare a leggere un file di testo in Rust, devi prima accedere alla libreria standard di Rust, che contiene diverse funzioni utili per manipolare i file. La più importante è `std::fs`, che ti permette di aprire e leggere file.

Per aprire un file in lettura, utilizzeremo la funzione `File::open()`, che prende come argomento il percorso del file che vogliamo aprire. Per esempio, se volessimo aprire il file "test.txt" nella cartella corrente, il codice sarebbe il seguente:

```Rust
use std::fs::File;

let file = File::open("test.txt").unwrap();
```

In questo esempio, stiamo dichiarando una variabile `file` che conterrà il nostro file aperto. Tieni presente che `unwrap()` è una funzione utilizzata per gestire gli errori, nel caso in cui qualcosa andasse storto durante l'apertura del file.

Una volta aperto il file, dobbiamo convertirlo in un'array di byte, utilizzando il metodo `read_to_end()`. In questo modo, possiamo leggere tutti i dati contenuti nel file in una sola volta. Ecco un esempio di codice completo:

```Rust
use std::fs::File;
use std::io::Read;

let mut file = File::open("test.txt").unwrap();
let mut bytes = Vec::new();

file.read_to_end(&mut bytes).unwrap();

println!("{:?}", bytes);
```

Una volta eseguito questo codice, dovresti ottenere un output simile al seguente:

```
[72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]
```

Questo è solo un esempio per mostrare come leggere i dati da un file, ma naturalmente puoi elaborare ulteriormente i dati e manipolarli in base alle tue esigenze specifiche.

## Approfondimento
Se vuoi approfondire maggiormente l'argomento, puoi prendere in considerazione la lettura dei file di testo in modo più specifico, come ad esempio leggere solo alcune linee del file o utilizzare codifiche diverse. Puoi anche esplorare la libreria `std::io` che contiene molte altre funzioni utili per lavorare con i file.

## Vedi anche
- [Documentazione ufficiale di Rust su `std::fs`](https://doc.rust-lang.org/std/fs/)
- [Esempi di codice su come leggere file di testo in Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=b37b5753d3d677ac4f396f84452ef8af)