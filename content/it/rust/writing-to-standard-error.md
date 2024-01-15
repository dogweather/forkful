---
title:                "Scrivere su standard error"
html_title:           "Rust: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è una pratica molto comune per tutti coloro che programmano in Rust. Questo perché è un modo efficace per visualizzare gli errori durante l'esecuzione del programma, consentendo agli sviluppatori di identificare e risolvere i problemi in modo più efficiente.

## Come Fare

Per scrivere su standard error in Rust, è possibile utilizzare la funzione `eprint!()` o `eprintln!()` a seconda della necessità. Queste funzioni accettano gli stessi argomenti di `print!()` e `println!()`, ma invece di stampare nella console standard, stampa il contenuto su standard error.

```rust
fn main() {
    let name = "Mario";
    eprintln!("Ciao, {}!", name);
}
```

Output:

```shell
Ciao, Mario!
```

## Approfondimento

Scrivere su standard error è utile in situazioni in cui si desidera mostrare un messaggio di errore al di fuori della console standard, ad esempio quando si sta lavorando con applicazioni multipiattaforma. Inoltre, è possibile specificare un colore diverso per i messaggi di errore su standard error utilizzando la crate `termcolor`.

## Vedi anche

- [Documentazione ufficiale di Rust su scrivere su standard error](https://doc.rust-lang.org/std/macro.eprint.html)
- [Guida alla crate `termcolor` per colorare il testo su standard error](https://docs.rs/termcolor/1.1.2/termcolor/)
- [Articolo su come gestire gli errori in Rust](https://www.davideaversa.it/2019/06/rust-errore-and-handling/)