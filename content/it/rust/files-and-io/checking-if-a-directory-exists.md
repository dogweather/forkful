---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:34.338711-07:00
description: "Come: La libreria standard di Rust (`std`) include funzionalit\xE0 per\
  \ controllare l'esistenza di una directory attraverso i moduli `std::path::Path`\
  \ e\u2026"
lastmod: '2024-03-13T22:44:43.232783-06:00'
model: gpt-4-0125-preview
summary: "La libreria standard di Rust (`std`) include funzionalit\xE0 per controllare\
  \ l'esistenza di una directory attraverso i moduli `std::path::Path` e `std::fs`."
title: Verifica se una directory esiste
weight: 20
---

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
