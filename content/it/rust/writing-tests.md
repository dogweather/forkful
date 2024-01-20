---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test significa creare codice per controllare se altri codici funzionano correttamente. I programmatori scrivono test per verificare automaticamente che il software si comporti come previsto, riducendo così errori e problemi futuri.

## Come fare:
Rust ha un sistema di testing integrato che permette di scrivere test unitari e d'integrazione semplicemente e velocemente.

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn it_fails() {
        assert_eq!(1 + 1, 3); // Questo test fallirà!
    }
}
```

Eseguendo `cargo test` otterrai qualcosa del genere:

```
running 2 tests
test tests::it_works ... ok
test tests::it_fails ... FAILED

failures:

---- tests::it_fails stdout ----
thread 'tests::it_fails' panicked at 'assertion failed: `(left == right)`
  left: `2`,
 right: `3`', src/lib.rs:10:9
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace


test result: FAILED. 1 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.02s
```

## Approfondimenti
Il sistema di test di Rust si ispira a framework esistenti in altri linguaggi, ma è progettato per integrarsi senza soluzione di continuità con il linguaggio stesso e il suo strumento di gestione dei pacchetti `cargo`. Altre alternative come `proptest` permettono di scrivere test basati su proprietà. I dettagli implementativi si trovano direttamente nel codice sorgente del compilatore di Rust e sono documentati ufficialmente in modo che i programmatori possano imparare ed estendere la funzionalità dei test.

## Vedi Anche
- [Rust Book - Testing](https://doc.rust-lang.org/book/ch11-00-testing.html) per una guida dettagliata sui test in Rust.
- [API Documentation for `std::assert!`](https://doc.rust-lang.org/std/macro.assert.html) per capire come utilizzare assert.
- [rust-lang/rust GitHub repository](https://github.com/rust-lang/rust) per esempi e discussioni sulla implementazione dei test.