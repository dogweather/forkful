---
aliases:
- /it/rust/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:24.480567-07:00
description: "Scrivere su standard error (stderr) in Rust significa indirizzare messaggi\
  \ di errore e diagnostica alla console separatamente dall'output standard\u2026"
lastmod: 2024-02-18 23:08:55.696026
model: gpt-4-0125-preview
summary: "Scrivere su standard error (stderr) in Rust significa indirizzare messaggi\
  \ di errore e diagnostica alla console separatamente dall'output standard\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) in Rust significa indirizzare messaggi di errore e diagnostica alla console separatamente dall'output standard (stdout). I programmatori fanno ciò per differenziare l'output normale del programma dai messaggi di errore, rendendo più facile gestire adeguatamente gli errori o reindirizzarli a registri o file durante l'esecuzione.

## Come fare:
Rust offre un modo diretto per scrivere su stderr utilizzando la macro `eprintln!`, simile a come `println!` viene usata per stdout. Ecco un esempio basilare:

```rust
fn main() {
    eprintln!("Questo è un messaggio di errore!");
}
```

Output di esempio (a stderr):
```
Questo è un messaggio di errore!
```

Per avere più controllo sui messaggi di errore, come quando si desidera formattare il testo o gestire i risultati I/O, utilizzare la funzione `stderr` dal modulo `std::io`. Questo metodo fornisce un handle al flusso globale stderr, sul quale è possibile scrivere utilizzando metodi come `write_all` o `writeln` dal trait `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Messaggio di errore formattato: {}", 404).expect("Impossibile scrivere su stderr");
}
```

Output di esempio (a stderr):
```
Messaggio di errore formattato: 404
```

Se stai lavorando in ambienti o applicazioni dove fai affidamento su librerie per il logging o la gestione degli errori, librerie come `log` e `env_logger` sono popolari. Anche se sono usate più per scopi di logging, sono configurabili e possono indirizzare i livelli di log degli errori a stderr. Di seguito è riportato un esempio di utilizzo semplice con `log` e `env_logger`:

Prima, aggiungi le dipendenze al tuo `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Poi, configura e usa il logging nella tua applicazione:
```rust
fn main() {
    env_logger::init();
    log::error!("Questo è un messaggio di errore loggato su stderr");
}
```

Eseguendo questo programma (dopo aver configurato `env_logger` con una variabile di ambiente appropriata, per esempio, `RUST_LOG=error`) verrà visualizzato il messaggio di errore su stderr, utilizzando l'infrastruttura di logging.

```plaintext
ERROR: Questo è un messaggio di errore loggato su stderr
```
