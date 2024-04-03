---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:16.188032-07:00
description: "Come fare: La libreria standard di Rust offre strumenti robusti per\
  \ la manipolazione dei file, principalmente incapsulati nei moduli `std::fs` e\u2026"
lastmod: '2024-03-13T22:44:43.236603-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Rust offre strumenti robusti per la manipolazione
  dei file, principalmente incapsulati nei moduli `std::fs` e `std::io`.
title: Scrivere un file di testo
weight: 24
---

## Come fare:
La libreria standard di Rust offre strumenti robusti per la manipolazione dei file, principalmente incapsulati nei moduli `std::fs` e `std::io`. Ecco un esempio base per creare e scrivere un file di testo:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Ciao, mondo!")?;
    Ok(())
}
```

Dopo aver eseguito questo codice, troverai un file denominato `hello.txt` con il contenuto "Ciao, mondo!".

Per scenari più complessi, come l'aggiunta a un file o la gestione efficiente di dati più grandi, Rust offre funzionalità aggiuntive. Ecco come aggiungere testo a un file esistente:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Aggiungendo altro testo.")?;
    Ok(())
}
```

Eseguendo questo, si aggiungerà " Aggiungendo altro testo." alla fine di `hello.txt`.

In alcuni casi, sfruttare le librerie di terze parti può semplificare le operazioni sui file. La crate `serde`, combinata con `serde_json`, per esempio, consente di serializzare e deserializzare strutture dati da e verso il formato JSON, offrendo un approccio ad alto livello alla scrittura di file:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

Dopo aver eseguito il codice sopra, `user.json` conterrà una rappresentazione in formato JSON della struct `User`. Da notare che l'utilizzo di `serde` e `serde_json` richiede di aggiungere queste crate al proprio `Cargo.toml`.

Scrivere file di testo in Rust, sia tramite la libreria standard sia con l'aiuto di crate esterne, è un modo semplice ma potente per gestire la persistenza dei dati nelle vostre applicazioni.
