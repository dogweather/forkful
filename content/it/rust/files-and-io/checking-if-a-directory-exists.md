---
title:                "Verifica se una directory esiste"
aliases:
- /it/rust/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:34.338711-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Nello sviluppo software, spesso è necessario verificare se una directory esiste per evitare errori quando si tenta di accedere, leggere o scrivere file. Rust, essendo un linguaggio di programmazione di sistema, offre metodi robusti per eseguire questo compito, garantendo che il tuo programma possa gestire file e directory in modo sicuro ed efficiente.

## Come:
La libreria standard di Rust (`std`) include funzionalità per controllare l'esistenza di una directory attraverso i moduli `std::path::Path` e `std::fs`. Ecco un esempio semplice utilizzando l'approccio standard di Rust:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/percorso/alla/directory");
    if path.exists() && path.is_dir() {
        println!("La directory esiste.");
    } else {
        println!("La directory non esiste.");
    }
}
```

Output di esempio, assumendo che la directory esista:
```
La directory esiste.
```

Per scenari più complessi o funzionalità avanzate (come le operazioni asincrone sul file system), potresti considerare l'uso di una libreria di terze parti come `tokio` con il suo modulo `fs` asincrono, specialmente se stai lavorando all'interno di un runtime asincrono. Ecco come potresti ottenere lo stesso risultato con `tokio`:

Prima, aggiungi `tokio` al tuo `Cargo.toml`:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Poi, usa `tokio::fs` per verificare se una directory esiste in modo asincrono:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/percorso/alla/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("La directory esiste.");
            } else {
                println!("Il percorso esiste ma non è una directory.");
            }
        },
        Err(_) => println!("La directory non esiste."),
    }
}
```

Output di esempio, assumendo che la directory non esista:
```
La directory non esiste.
```

Questi esempi evidenziano come Rust e il suo ecosistema offrano approcci sia sincroni che asincroni per la verifica dell'esistenza di directory, soddisfacendo una vasta gamma di esigenze di sviluppo software.
