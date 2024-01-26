---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere sull'errore standard significa mandare messaggi di errore o log al flusso stderr, diverso dall'output standard (stdout). Lo facciamo per separare l'output normale dai messaggi di errore, permettendo di gestirli separatamente.

## How to:
Usa `eprintln!` per scrivere su stderr, e `println!` per stdout. Ecco un esempio:

```Rust
fn main() {
    println!("Questo è un messaggio standard."); // Verrà scritto su stdout
    eprintln!("Questo è un messaggio di errore."); // Verrà scritto su stderr
}
```

Output di `stdout`:
```
Questo è un messaggio standard.
```

Output di `stderr`:
```
Questo è un messaggio di errore.
```

## Deep Dive
Histricamente, la separazione tra stdout e stderr esiste per permettere agli utenti di redirigere questi due tipi di output in file o comandi diversi. In Rust, puoi usare `std::io::stderr()` per gestire manualmente il flusso e scrivere con metodi come `.write_all()`. In alternativa, librerie esterne offrono maggiore flessibilità e funzionalità per la gestione degli errori e dei log.

## See Also
- [La documentazione ufficiale Rust sull'uso di `println!` e `eprintln!`](https://doc.rust-lang.org/std/macro.println.html)
- [Guida alla libreria `std::io`](https://doc.rust-lang.org/std/io/index.html)
- [Serde](https://serde.rs/) per la serializzazione dei dati, utile nel logging avanzato.
- [Log](https://docs.rs/log/) per un framework di logging estensibile.
