---
title:    "Rust: Verifica dell'esistenza di una directory."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Una delle sfide più comuni nella programmazione è la gestione dei file e delle cartelle. Spesso, i programmatori si trovano a dover controllare se una determinata cartella esiste o meno, prima di poter eseguire delle operazioni su di essa. In questo articolo, verrà presentato come verificare la presenza di una directory utilizzando il linguaggio Rust.

## Come fare

In Rust, il modo più semplice per verificare l'esistenza di una cartella è utilizzare la funzione `exists()` del modulo `std::fs`. Questa funzione accetta un percorso di tipo `&Path` e restituisce un valore booleano, `true` se la cartella esiste, `false` altrimenti.

```rust
use std::fs;

fn main() {
    let path = "C:/Users/Utente/Documents/esempio";

    if fs::exists(path) {
        println!("La cartella esiste!");
    } else {
        println("La cartella non esiste.");
    }
}
```

Se si desidera ottenere informazioni più dettagliate sulla cartella, è possibile utilizzare la funzione `metadata()` del modulo `std::fs`. Questa funzione restituisce un oggetto di tipo `std::fs::Metadata`, il quale contiene informazioni come la dimensione, i permessi e la data di ultima modifica della cartella.

```rust
use std::fs;

fn main() {
    let path = "C:/Users/Utente/Documents/esempio";

    match fs::metadata(path) {
        Ok(metadata) => {
            println!("Dimensione: {} byte", metadata.len());
            println!("Permessi: {:?}", metadata.permissions());
            println!("Ultima modifica: {:?}", metadata.modified());
        }
        Err(_) => {
            println!("La cartella non esiste o non si ha accesso ad essa.");
        }
    }
}
```

## Approfondimento

Sotto il cofano, la funzione `exists()` utilizza la struttura `metadata()` per controllare se una cartella esiste. Tuttavia, se si desidera evitare un doppio accesso al file system, è possibile utilizzare direttamente `metadata()` e gestire gli eventuali errori.

```rust
use std::fs::metadata;
use std::path::Path;

fn main() {
    let path = Path::new("C:/Users/Utente/Documents/esempio");

    match metadata(path) {
        Ok(metadata) => {
            // Gestione delle informazioni sulla cartella
        }
        Err(_) => {
            // Gestione dell'errore
        }
    }
}
```

## Vedi anche

- Documentazione di Rust su `fs::exists()` e `fs::metadata()`: https://doc.rust-lang.org/std/fs/fn.exists.html, https://doc.rust-lang.org/std/fs/fn.metadata.html
- Tutorial su come gestire i file e le cartelle in Rust: https://www.geeksforgeeks.org/file-handling-in-rust/
- Esempi pratici di come utilizzare le funzioni `exists()` e `metadata()`: https://www.w3adda.com/rust-tutorial/rust-functions-exists-metadata