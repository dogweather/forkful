---
title:    "Rust: Ottenere la data odierna"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

La gestione della data corrente è un aspetto essenziale della programmazione ed è fondamentale per molte applicazioni. Con Rust, puoi ottenere la data e l'ora attuali in modo efficiente e preciso, senza dover fare affidamento su librerie esterne.

## Come fare

Ecco un esempio di come ottenere la data corrente in Rust:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let seconds_since_epoch = now.duration_since(UNIX_EPOCH)
    .expect("Errore nel calcolo del tempo");
let current_timestamp = seconds_since_epoch.as_secs();
println!("Unix timestamp attuale: {}", current_timestamp);
```

Eseguendo questo codice, otterrai l'Unix timestamp attuale, mostrando il numero di secondi trascorsi dalla mezzanotte del 1 gennaio 1970. Puoi anche utilizzare la libreria `chrono` per formattare la data e l'ora in modo più leggibile.

## Approfondimenti

Ottieni una maggiore comprensione di come funziona la gestione della data corrente in Rust con questi approfondimenti:

- [Documentazione ufficiale su `std::time`](https://doc.rust-lang.org/std/time/index.html)
- [Tutorial su come utilizzare `chrono`](https://www.oreilly.com/library/view/rust-programming-by/9781788390637/643fb8f5-6c78-4588-bcbe-f059a9ca7d4d.xhtml)
- [Articolo su come gestire il tempo in Rust](https://www.digitalocean.com/community/tutorials/how-to-handle-datetime-in-rust)

## Vedi anche

- [Documentazione ufficiale di Rust](https://www.rust-lang.org/it)
- [Cosa è Rust e perché dovresti impararlo](https://www.rust-lang.org/it/what-is-rust)
- [Video tutorial su Rust per principianti](https://www.youtube.com/watch?v=U1EFgCNLDB8)