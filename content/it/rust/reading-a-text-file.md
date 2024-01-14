---
title:    "Rust: Lettura di un file di testo"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore curioso e vuoi imparare un nuovo linguaggio di programmazione, allora è importante saper leggere un file di testo. Questa abilità è fondamentale per molte attività di programmazione, come l'analisi dei dati o la creazione di report.

## Come fare

La buona notizia è che leggere un file di testo in Rust è abbastanza semplice e intuitivo. Iniziamo con il codice base:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Apre un nuovo file in lettura
    let mut file = File::open("test.txt").expect("Impossibile aprire il file");

    let mut content = String::new();
    // Legge tutto il contenuto del file
    file.read_to_string(&mut content)
        .expect("Impossibile leggere il file");

    // Stampa il contenuto del file
    println!("{}", content);
}
```

In questo esempio, abbiamo importato i moduli `std::fs` e `std::io` che ci permettono di lavorare con i file e la gestione dei dati di input/output. Inoltre, abbiamo creato una nuova istanza `File` chiamata `file` che punta al nostro file di testo "test.txt". Per leggere il contenuto del file, utilizziamo il metodo `read_to_string` che restituisce una `String` contenente il contenuto del file. Infine, stampiamo il contenuto del file sulla console.

Se vogliamo leggere il contenuto del file riga per riga, possiamo utilizzare il metodo `lines()` come nell'esempio seguente:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("test.txt").expect("Impossibile aprire il file");

    // Legge ogni riga del file e la inserisce in un vettore
    let lines: Vec<String> = file
        .lines()
        .filter_map(|line| line.ok())
        .collect();

    // Stampa il contenuto del vettore
    println!("{:?}", lines);
}
```

In questo caso, abbiamo utilizzato il metodo `lines()` che restituisce un iteratore che scorre ogni riga del file. Con l'aiuto del metodo `filter_map`, abbiamo eliminato eventuali righe danneggiate e creato un vettore contenente le righe valide. Infine, abbiamo stampato il contenuto del vettore sulla console.

## Approfondimento

Ora che hai imparato come leggere un file di testo in Rust, è importante anche conoscere le diverse opzioni disponibili per la gestione dei file. Ad esempio, puoi utilizzare la macro `println!` per scrivere il contenuto del file direttamente sulla console senza utilizzare il metodo `read_to_string`. Inoltre, puoi gestire anche eventuali errori con l'utilizzo di `match` o `if let` per gestire gli errori di apertura o lettura del file.

Per ulteriori informazioni, consulta la documentazione ufficiale di Rust su come gestire i file: [https://doc.rust-lang.org/std/fs/index.html](https://doc.rust-lang.org/std/fs/index.html)

## Vedi anche

- [Rust By Example - File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [Rust Book - Working with Files](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [The Reference - File I/O](https://doc.rust-lang.org/reference/io.html)