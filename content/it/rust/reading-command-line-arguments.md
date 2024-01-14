---
title:                "Rust: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori scelgono di leggere gli argomenti della riga di comando prima di eseguire un programma per personalizzare l'esecuzione o per fornire input aggiuntivi. In questo post, vi mostrerò come leggere gli argomenti della riga di comando in Rust.

## Come fare

Per leggere gli argomenti della riga di comando in Rust, possiamo utilizzare la funzione `std::env::args()` della libreria standard. Questa funzione restituisce un iteratore che contiene tutti gli argomenti passati al programma. Possiamo utilizzare il metodo `collect()` per convertire questo iteratore in un vettore.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Gli argomenti passati al programma sono: {:?}", args);
}
```

L'esempio sopra stampa tutti gli argomenti passati al programma quando viene eseguito da riga di comando. Per esempio, se eseguiamo il programma con il comando `cargo run hello world`, l'output sarà:

```
Gli argomenti passati al programma sono: ["hello", "world"]
```

Inoltre, possiamo utilizzare il metodo `len()` sul vettore per ottenere il numero di argomenti passati.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Il numero di argomenti è: {}", args.len());
}
```

Questo ci restituirà il numero di argomenti passati al programma.

## Deep Dive

La funzione `std::env::args()` restituisce un iteratore che contiene anche il nome del programma come primo argomento. Quindi, se eseguiamo il programma con il comando `cargo run`, il vettore conterrà un solo elemento, il nome del programma.

Inoltre, possiamo anche utilizzare il metodo `nth()` per accedere a un argomento specifico. Questo metodo accetta un intero come parametro che rappresenta la posizione dell'argomento da recuperare. Ad esempio, se vogliamo recuperare il secondo argomento, possiamo utilizzare `args.nth(2)`.

## Vedi anche

- [Documentazione ufficiale di Rust - Gestione degli argomenti della riga di comando](https://doc.rust-lang.org/std/env/fn.args.html)
- [Come utilizzare gli argomenti della riga di comando in Rust](https://www.section.io/engineering-education/command-line-arguments-in-rust/)
- [Esempi di codice di utilizzo degli argomenti della riga di comando in Rust](https://www.codota.com/code/rust/functions/std::env::args)