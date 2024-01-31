---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare i dati in un formato leggibile dall'uomo. I programmatori lo fanno per persistere informazioni, configurazioni, o per registrare attività di log.

## How to:
```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut file = File::create("esempio.txt")?;
    file.write_all(b"Ciao, mondo!")?;
    Ok(())
}
```
Questo codice crea un file chiamato "esempio.txt" e ci scrive dentro "Ciao, mondo!".

## Deep Dive
La scrittura di file di testo in Rust è un'operazione comune fin dai primi giorni del linguaggio. Importanti alternative includono le funzionalità della libreria standard come `std::fs::write` per operazioni one-liner. Rust assicura la sicurezza attraverso i check a compile time per evitare errori come il data race in operazioni di I/O.

## See Also
- [std::fs Module Documentation](https://doc.rust-lang.org/std/fs/index.html)
- [Rust By Example – File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
