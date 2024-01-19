---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Iniziare un Nuovo Progetto con Rust: un Tutorial Facile e Rapido

## Cosa e Perché?

Avviare un nuovo progetto è creare un ambiente di lavoro da zero per un tuo software. Gli sviluppatori fanno ciò per organizzare il codice in modo efficiente, rendendolo più gestibile e collaborativo.

## Come fare:

Inizia un nuovo progetto utilizzando il comando `cargo new`. Vediamo come si fa:

```Rust
$ cargo new mio_progetto
```

Dopo aver eseguito questo comando, avrai una nuova cartella `mio_progetto` con una struttura simile a questa:

```Rust
mio_progetto/
 ├── Cargo.toml
 └── src
     └── main.rs
```

Il codice nel file main.rs sarà:

```Rust
fn main() {
    println!("Ciao, mondo!");
}
```

Eseguendo questo codice con `cargo run` otterresti:

```Rust
$ cargo run
   Compiling mio_progetto v0.1.0 (/path/to/mio_progetto)
    Finished dev [unoptimized + debuginfo] target(s) in 2.82s
     Running `target/debug/mio_progetto`
Ciao, mondo!
```

## Approfondimento

L'ambiente Rust e il suo sistema di gestione dei pacchetti `cargo` sono stati rilasciati nel 2015. Da allora, ha offerto una robusta opzione per la gestione del progetto e il controllo delle versioni.

Alcuni lingue e ambienti, come Node.js con npm, Ruby con RubyGems, offrono funzionalità simili. Ma la costruzione sicura, l'immutabilità e il controllo delle dipendenze di Rust lo distinguono.

Alla base, `cargo new` crea un "Crates", che è un pacchetto di Rust. Ogni Crates ha un file TOML, che elenca le sue dipendenze. Questo facilita la condivisione e l'uso di diverse librerie.

## Vedi Anche

1. [Il Libro di Rust](https://doc.rust-lang.org/book/title-page.html) (Inglese)

2. [Rust by Example](https://doc.rust-lang.org/rust-by-example/) (Inglese)

3. [Api di Rust](https://doc.rust-lang.org/std/) (Inglese)

Capire come avviare un nuovo progetto in Rust è solo l'inizio. Continua a imparare e a sperimentare.  Buona programmazione in Rust!