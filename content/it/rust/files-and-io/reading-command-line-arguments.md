---
date: 2024-01-20 17:56:56.793539-07:00
description: "Leggere gli argomenti della riga di comando significa raccogliere input\
  \ dall'esterno quando il programma viene eseguito. I programmatori lo fanno per\u2026"
lastmod: '2024-02-25T18:49:41.108384-07:00'
model: gpt-4-1106-preview
summary: "Leggere gli argomenti della riga di comando significa raccogliere input\
  \ dall'esterno quando il programma viene eseguito. I programmatori lo fanno per\u2026"
title: Lettura degli argomenti della riga di comando
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando significa raccogliere input dall'esterno quando il programma viene eseguito. I programmatori lo fanno per personalizzare l'esecuzione del programma senza modificare il codice.

## How to:
Ecco come leggere gli argomenti della riga di comando in Rust:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // Stampa tutti gli argomenti della riga di comando
    for (index, arg) in args.iter().enumerate() {
        println!("Argomento {}: {}", index, arg);
    }
}
```

Eseguendo `programma arg1 arg2`, l'output sarà:

```
Argomento 0: programma
Argomento 1: arg1
Argomento 2: arg2
```

## Deep Dive
Leggere gli argomenti della riga di comando è un concetto antico quanto i primi sistemi operativi Unix. `std::env::args` gestisce gli argomenti in Rust. Se hai bisogno di qualcosa di più sofisticato per gestire gli argomenti, prova `clap` o `structopt`.

Mentre `env::args` ti dà gli argomenti così come sono, `clap` e `structopt` consentono di definire complessi schemi di parsing, messaggi di aiuto e molti altri strumenti utili che semplificano la vita quando si gestiscono opzioni più complesse.

Tradizionalmente, l'argomento indice 0 è il percorso al programma stesso, mentre gli argomenti successivi sono quelli forniti dall'utente.

## See Also
- [The Rust Programming Language - Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [clap documentation](https://docs.rs/clap/)
- [structopt documentation](https://docs.rs/structopt/0.3.21/structopt/)
