---
title:                "Rust: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile in situazioni in cui è necessario mostrare messaggi di errore o di debug durante l'esecuzione di un programma Rust. Invece di interrompere il programma o stampare i messaggi su standard output, si può scrivere su standard error per distinguere chiaramente i messaggi di errore dai dati di output.

## Come fare

Per scrivere su standard error in Rust, si può utilizzare la funzione `eprintln!()` seguita da ciò che si desidera stampare su standard error all'interno delle parentesi graffe. Ad esempio:

```Rust
fn main() {
    let value = 10;
    eprintln!("Il valore è: {}", value);
}
```

Questo creerà una nuova riga su standard error contenente il messaggio `Il valore è: 10`. Si possono utilizzare anche formattazioni specifiche all'interno delle parentesi graffe, come `%d` per valori interi o `%f` per numeri decimali.

## Approfondimenti

Scrivere su standard error in Rust è importante quando si vogliono ottenere messaggi di errore chiari e facilmente distinguibili dal resto dell'output del programma. Inoltre, è utile per il debug del codice, in quanto i messaggi di errore verranno visualizzati anche in caso di crash del programma.

Si consiglia di utilizzare `eprintln!()` invece di `println!()` per stampare su standard error, in quanto l'ultimo può essere sovrascritto da altre librerie o framework utilizzati nel programma.

## Vedi anche

- [Guida ai formati di output in Rust](https://doc.rust-lang.org/std/fmt/)
- [Documentazione ufficiale di Rust](https://www.rust-lang.org/it/learn)
- [Rust Cookbook: Debugging e stampa su standard error](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging/stderr.html)