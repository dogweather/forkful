---
title:                "Rust: Lettura degli argomenti della riga di comando"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Scrivere programmi efficienti e affidabili è di fondamentale importanza in qualsiasi linguaggio di programmazione. Ecco perché, se sei un programmatore in Rust, è utile conoscere come leggere gli argomenti della riga di comando. Questo ti permetterà di creare programmi più flessibili e interattivi, che possono essere facilmente personalizzati dagli utenti.

## Come fare

In Rust, leggere gli argomenti della riga di comando è abbastanza semplice. Per prima cosa, dobbiamo importare la libreria "std::env". Qui di seguito puoi vedere un esempio di codice:

```Rust
use std::env;

fn main() {
    // Leggi gli argomenti della riga di comando
    let arguments: Vec<String> = env::args().collect();

    // Stampa il numero totale di argomenti
    println!("Il programma ha ricevuto {} argomenti.", arguments.len());

    // Stampa ogni argomento
    for argument in arguments {
        println!("{}", argument);
    }
}
```

Ecco un esempio di output per il programma sopra:

```Rust
$ ./programma arg1 arg2 arg3
Il programma ha ricevuto 4 argomenti.
./programma
arg1
arg2
arg3
```

Come puoi vedere, la funzione "args()" della libreria std::env ci restituisce un vettore contenente tutti gli argomenti della riga di comando. Possiamo quindi accedere a ogni argomento usando un ciclo "for".

## Deep Dive

Ora, supponiamo di voler leggere un argomento specifico dalla riga di comando, come ad esempio il terzo argomento. In questo caso, possiamo utilizzare il seguente codice:

```Rust
use std::env;

fn main() {
    // Leggi gli argomenti della riga di comando
    let arguments: Vec<String> = env::args().collect();

    // Accedi al terzo argomento (indice 2 poiché l'indice inizia da 0)
    let third_argument = &arguments[2];

    // Stampa il terzo argomento
    println!("Il terzo argomento è: {}", third_argument);
}
```

Inoltre, possiamo anche fornire dei valori di default per gli argomenti che non vengono specificati dall'utente. Per farlo, possiamo utilizzare la funzione "args_os()" invece di "args()". Questa funzione ci restituisce un iteratore sugli argomenti, che possiamo utilizzare per scorrere tutti gli argomenti specificati dall'utente e poi fornire i valori di default per quelli mancanti.

## Vedi anche

- Documentazione ufficiale di Rust per leggere gli argomenti della riga di comando: https://doc.rust-lang.org/std/env/fn.args.html
- Un tutorial completo su come leggere gli argomenti della riga di comando in Rust: https://www.tutorialspoint.com/how-to-read-command-line-arguments-in-rust-programming