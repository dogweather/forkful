---
title:                "Scrivere test"
html_title:           "Rust: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test è importante

Se sei un programmatore, probabilmente hai sentito parlare dell'importanza di scrivere test per il tuo codice. Ma perché è così importante? In breve, i test ci permettono di verificare che il nostro codice funzioni correttamente e che non si verifichino errori inaspettati quando lo aggiorniamo o lo modifichiamo. Inoltre, i test ci aiutano a rilevare più facilmente e correggere eventuali errori di codice.

## Come scrivere test in Rust

In Rust, i test sono inclusi all'interno dei file di codice tramite l'attributo `#[test]`. Possiamo utilizzare l'assertione `assert!()` per verificare che un'espressione booleana sia vera. Ecco un esempio:

```Rust
#[test]
fn test_somma() {
    let x = 2;
    let y = 3;

    // Verifichiamo che la somma di x e y sia uguale a 5
    assert!(x + y == 5);
}
```

Una volta scritti i test all'interno dei nostri file di codice, possiamo eseguirli utilizzando il comando `cargo test`. Verranno eseguiti tutti i test presenti nei file del progetto e otterremo un output simile a questo:

```sh
running 1 test
test test_somma ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Se uno dei nostri test fallisce, otterremo un output diverso che ci aiuterà a identificare l'errore. Ad esempio:

```sh
running 1 test
test test_somma ... FAILED

failures:

---- test_somma stdout ----
thread 'test_somma' panicked at 'assertion failed: `(left == right)`
  left: `6`,
 right: `5`', src/main.rs:6

failures:
    test_somma

test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

## Approfondimenti sui test in Rust

Per scrivere test efficaci in Rust, è importante comprendere i concetti di ***testing framework*** e ***unit testing***. Un testing framework è un insieme di strumenti e funzionalità che ci aiutano a gestire e eseguire i nostri test. Unit testing, invece, si riferisce alla pratica di testare le unità di codice, ovvero le singole funzioni o moduli, per assicurarsi che funzionino correttamente.

Inoltre, in Rust possiamo utilizzare il costrutto `#[should_panic]` per testare che una determinata funzione generi un errore specifico. Vediamo un esempio:

```Rust
#[test]
fn test_divisione() {
    let x = 2;
    let y = 0;

    // La divisione per 0 deve generare un errore
    assert_eq!(x / y, 0);
}

#[test]
#[should_panic] // Questo test deve generare un errore
fn test_scelta_lista_vuota() {
    let lista = Vec::new();

    // Tentiamo di selezionare un elemento dalla lista vuota
    lista[0];
}
```

Con questi strumenti, possiamo iniziare a scrivere test per il nostro codice in modo più efficace e migliorare la qualità del nostro software.

## Vedi anche

- [La documentazione ufficiale di Rust sui test](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Un tutorial dettagliato sui test in Rust (in inglese)](https://blog.rust-lang.org/inside-rust/2020/06/08/test-suite.html)
- [Un video tutorial sui test in Rust (in inglese)](https://www.youtube.com/watch?v=EZetWEPW6Ec&ab_channel=mearan)