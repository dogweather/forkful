---
title:    "Rust: Lettura degli argomenti della riga di comando"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando è un'operazione fondamentale per qualsiasi programmazione su Rust. Non solo è utile per la comunicazione con l'utente, ma può anche fornire informazioni importanti per il corretto funzionamento del programma.

## Come fare

Per leggere gli argomenti della riga di comando in Rust, è necessario utilizzare la libreria standard `std::env`. Questo modulo fornisce la funzione `args` che restituisce un iterator sugli argomenti della riga di comando. Ad esempio, se vogliamo stampare tutti gli argomenti passati al nostro programma, possiamo utilizzare il seguente codice:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    for arg in args {
        println!("{}", arg);
    }
}
```

Proviamo a eseguire questo programma con alcuni argomenti:

```bash
$ cargo run hello world
```

L'output sarà:

```
example
hello
world
```

Come possiamo vedere, il primo argomento è sempre il nome del programma stesso, seguito dagli argomenti passati dall'utente. Inoltre, possiamo utilizzare il metodo `len` sul vettore degli argomenti per ottenere il numero totale di argomenti passati.

## Approfondimento

Oltre alla funzione `args`, il modulo `std::env` offre anche altre utili funzioni per la gestione degli argomenti della riga di comando. Ad esempio, possiamo utilizzare il metodo `current_exe` per ottenere il percorso del programma in esecuzione, o il metodo `var` per ottenere il valore di una variabile di ambiente specifica. È anche possibile modificare le variabili di ambiente utilizzando i metodi `set_var` e `remove_var`.

Inoltre, è possibile utilizzare la libreria esterna `clap` per gestire in modo più strutturato e flessibile gli argomenti della riga di comando.

## Vedi anche

- Documentazione ufficiale di `std::env`: https://doc.rust-lang.org/std/env/
- Documentazione di `clap`: https://docs.rs/clap/3.0.0-beta.3/clap/