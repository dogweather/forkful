---
title:                "Verifica se esiste una cartella"
html_title:           "Rust: Verifica se esiste una cartella"
simple_title:         "Verifica se esiste una cartella"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Controllare se una directory esiste è un'operazione comune che i programmatori spesso fanno per verificare l'accesso a una specifica directory del file system. Questo è utile per garantire che il codice funzioni correttamente e prevenire eventuali errori.

## Come:
Ecco un semplice esempio di codice in Rust che controlla se una directory esiste utilizzando la libreria standard di Rust:

```Rust
use std::fs;
 
fn main() {
    let directory = fs::metadata("nome_directory");
 
    match directory {
        Ok(_) => println!("La directory esiste!"),
        Err(_) => println!("La directory non esiste :("),
    }
}
```

L'esempio sopra utilizza la funzione `metadata()` della libreria standard di Rust per ottenere informazioni sulla directory specificata. Se la chiamata restituisce un risultato `Ok`, significa che la directory esiste, altrimenti restituisce un `Err`. È importante notare che questo metodo funziona solo su directory esistenti nel file system.

## Approfondimento:
In passato, il modo più comune di controllare se una directory esiste era utilizzare la funzione `opendir()` della libreria standard di C. Tuttavia, questa approccio presentava alcuni problemi, come la mancanza di informazioni precise per distinguere tra un errore causato da una directory inesistente e un altro tipo di errore.

In alternativa, è possibile utilizzare librerie di terze parti più specializzate, come "fs_extra" o "walkdir", che offrono maggiori funzionalità e controlli di errore più precisi.

Per quanto riguarda l'implementazione interna di Rust, la funzione `metadata()` esegue una chiamata al sistema operativo per ottenere informazioni sulla directory specificata. Se la directory esiste, questa chiamata restituirà un risultato `Ok` altrimenti restituirà un `Err`.

## Vedi anche:
- Documentazione della libreria standard di Rust: https://doc.rust-lang.org/std/fs/fn.metadata.html
- Libreria "fs_extra": https://crates.io/crates/fs_extra
- Libreria "walkdir": https://crates.io/crates/walkdir