---
date: 2024-01-26 00:57:55.278309-07:00
description: "La gestione degli errori riguarda il come affrontare le situazioni quando\
  \ le cose non vanno come previsto. I programmatori lo fanno per gestire\u2026"
lastmod: '2024-02-25T18:49:41.100555-07:00'
model: gpt-4-1106-preview
summary: "La gestione degli errori riguarda il come affrontare le situazioni quando\
  \ le cose non vanno come previsto. I programmatori lo fanno per gestire\u2026"
title: Gestione degli errori
---

{{< edit_this_page >}}

## Cosa & Perché?

La gestione degli errori riguarda il come affrontare le situazioni quando le cose non vanno come previsto. I programmatori lo fanno per gestire l'inaspettato, assicurandosi che i loro programmi in Rust siano robusti e non si blocchino semplicemente di fronte a un intoppo.

## Come fare:

Rust gestisce gli errori in due modi principali: errori recuperabili e non recuperabili. Vediamoli entrambi.

Gli errori recuperabili utilizzano `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("File aperto con successo."),
        Err(_e) => println!("Apertura del file fallita."),
    }
}
```

L'output potrebbe essere "File aperto con successo." o "Apertura del file fallita.", a seconda del tuo `hello.txt`.

Per gli errori non recuperabili, usiamo `panic!`:

```Rust
fn main() {
    // Questo causerà il panic del programma perché probabilmente il file non esiste.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Eseguilo, e vedrai un messaggio di panic. Il tuo programma si ferma all'istante.

## Approfondimento

Storicamente, la gestione degli errori nella programmazione è stata complicata. Rust la rende chiara con una distinzione netta tra errori recuperabili e non recuperabili.

L'enum `Result` è per gli errori recuperabili. È esplicito - gestisci la variante `Ok` o `Err`. Hai anche metodi come `unwrap()` ed `expect()`, ma sono scorciatoie veloci e sporche che possono portare a un `panic!`.

`panic!` è il modo in cui Rust esprime che qualcosa di davvero brutto è successo e non può farci nulla. È come un errore non recuperabile che ferma immediatamente l'esecuzione. Un panic in Rust è spesso associato a bug che non ti aspetti di gestire, come l'indicizzazione fuori dai limiti di un array.

La gestione degli errori tramite il ritorno di `Result` è preferita quando ci si aspetta di dover gestire degli errori. È idiomatico in Rust, il che significa che è il modo in cui gli sviluppatori di Rust hanno concordato di fare le cose. Esiste anche `Option<T>`, per i casi in cui un errore è semplicemente qualcosa che è `None` invece di `Some(T)`. Si tratta di aspettarsi l'inaspettato senza paura.

Alternative? Certo, potresti usare altri crate di gestione degli errori per più funzionalità o uso ergonomico. Come `anyhow` per una gestione degli errori semplice, o `thiserror` per gli errori nel codice di librerie.

## Vedi Anche

Interessato ad approfondire? Ecco dove andare:

- [Il Libro di Rust sulla Gestione degli Errori](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Un ottimo punto di partenza per comprendere la filosofia di Rust nella gestione degli errori.
- [Rust per Esempio: Gestione degli errori](https://doc.rust-lang.org/rust-by-example/error.html) - Esempi interattivi per mettere le mani in pasta.

Ricorda, una buona gestione degli errori non è solo codificare; è prendersi cura degli utenti del tuo codice. Buona programmazione!
