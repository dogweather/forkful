---
date: 2024-01-20 18:04:17.026416-07:00
description: "Partire con un nuovo progetto \xE8 eccitante; \xE8 l'atto di creare\
  \ un ambiente di codice da zero. I programmatori lo fanno perch\xE9 vogliono trasformare\
  \ le idee\u2026"
lastmod: '2024-03-13T22:44:43.218721-06:00'
model: gpt-4-1106-preview
summary: "Partire con un nuovo progetto \xE8 eccitante; \xE8 l'atto di creare un ambiente\
  \ di codice da zero."
title: Avvio di un nuovo progetto
weight: 1
---

## How to:
Per iniziare un nuovo progetto in Rust, nulla di più semplice. Rust utilizza `cargo`, il suo sistema di build e gestore di pacchetti. Ecco come configurarlo:

```rust
// Installa Rust e Cargo se non lo hai già fatto
// apri il terminale e digita:
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

// Crea un nuovo progetto Rust
$ cargo new mio_progetto
$ cd mio_progetto

// La struttura del progetto sarà così:
// mio_progetto/
// ├── Cargo.toml
// └── src
//     └── main.rs

// Esegui il progetto
$ cargo run

// Vedrai questo output, se tutto è andato bene:
   Compiling mio_progetto v0.1.0 (/path/to/mio_progetto)
    Finished dev [unoptimized + debuginfo] target(s) in 0.5 secs
     Running `target/debug/mio_progetto`
Hello, world!
```

## Deep Dive
Rust è un linguaggio progettato per la sicurezza e le prestazioni. Il progetto Rust iniziò nel 2006 da Graydon Hoare, e la prima versione stabile (1.0) fu rilasciata nel 2015. Cargo è parte integrante dell'ecosistema Rust e semplifica la gestione delle dipendenze e degli script di build.

Nel passato, i programmatori potevano avviare nuovi progetti manualmente, creando ogni file da zero. Con Rust e Cargo, questo processo è automazione pura: Cargo configura la struttura del progetto, il file `Cargo.toml` per le impostazioni del progetto e le dipendenze, e un semplice `main.rs` che stampa "Hello, World!".

Altre alternative includono l'uso di template preesistenti con Cargo, che ti permettono di avviare con un set preconfigurato di dependencies e struttura di progetto. Inoltre, comunità online, come GitHub e GitLab, offrono spesso "repository template" specifici per Rust.

## See Also
- La documentazione ufficiale di Rust: https://doc.rust-lang.org/book/
- Cargo, il gestore di pacchetti Rust: https://doc.rust-lang.org/cargo/
- tutorials introduttivi a Rust: https://www.rust-lang.org/learn
- Esempi di progetti Rust su GitHub: https://github.com/search?q=rust+example
