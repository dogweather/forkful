---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
La lettura degli argomenti della riga di comando consiste nel ricevere dati inseriti dall'utente direttamente al momento di eseguire un programma. Gli sviluppatori fanno ciò per migliorare l'interattività e l'adattabilità dei loro programmi.

## Come fare:
Utilizziamo il modulo `std::env` per leggere gli argomenti della riga di comando in Rust.

```rust
// importa il modulo std::env
use std::env;

fn main() {
    // usa args() per ottenere gli argomenti
    let args: Vec<String> = env::args().collect();

    // stampa ogni argomento
    for arg in args {
        println!("{}", arg);
    }
}
```
Quando eseguito con `cargo run arg1 arg2`, l'output sarà:

```shell
target/debug/my_program
arg1
arg2
```

## Approfondimento
(1) Contesto storico: La manipolazione degli argomenti della riga di comando è una caratteristica fondamentale dei linguaggi di programmazione sin dai primi giorni dello sviluppo del software. 
(2) Alternative: Sebbene `std::env::args` sia il modo consigliato per leggere gli argomenti della riga di comando in Rust, librerie esterne come `clap` e `argparse` offrono funzionalità più avanzate. 
(3) Dettagli di implementazione: `args()` restituisce un iteratore sugli argomenti della riga di comando. È importante notare che il primo argomento è sempre il percorso del programma stesso.

## Vedere anche
- Documentazione ufficiale di `std::env::args`: https://doc.rust-lang.org/std/env/fn.args.html
- Libreria `clap` di gestione degli argomenti: https://crates.io/crates/clap
- Libreria `argparse` di analisi degli argomenti: https://crates.io/crates/argparse