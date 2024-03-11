---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:48.111940-07:00
description: "Il recupero della data corrente in Rust \xE8 un compito comune per attivit\xE0\
  \ come la registrazione, operazioni basate sul tempo o semplicemente per\u2026"
lastmod: '2024-03-11T00:14:16.794143-06:00'
model: gpt-4-0125-preview
summary: "Il recupero della data corrente in Rust \xE8 un compito comune per attivit\xE0\
  \ come la registrazione, operazioni basate sul tempo o semplicemente per\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa & Perché?

Il recupero della data corrente in Rust è un compito comune per attività come la registrazione, operazioni basate sul tempo o semplicemente per visualizzare la data. A differenza di alcuni linguaggi che includono la funzionalità di data e ora nella loro libreria standard, Rust incoraggia l'uso di una libreria di terze parti robusta, chrono, per una manipolazione completa di data e ora, data la sua superiorità funzionale e facilità di uso.

## Come fare:

### Utilizzando la Libreria Standard di Rust
La libreria standard di Rust fornisce un modo limitato ma rapido per ottenere il tempo corrente, anche se non direttamente la data corrente in un formato calendario. Ecco come si fa:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Tempo attuale: {} secondi dall'epoca Unix.", n.as_secs()),
        Err(_) => panic!("SystemTime prima dell'epoca Unix!"),
    }
}
```

Output:
```
Tempo attuale: 1615390665 secondi dall'epoca Unix.
```

### Utilizzando la Libreria Chrono
Per una funzionalità di data e ora più completa, inclusa l'ottenimento della data corrente, dovresti usare la libreria `chrono`. Prima, aggiungi `chrono` al tuo `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Poi, puoi usare `chrono` per ottenere la data corrente:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Data corrente: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Output:
```
Data corrente: 2023-4-20
```

La libreria `chrono` rende semplice lavorare con date e orari, offrendo una vasta gamma di funzionalità oltre al semplice recupero della data corrente, inclusa l'analisi, la formattazione e le operazioni aritmetiche su date e orari.
