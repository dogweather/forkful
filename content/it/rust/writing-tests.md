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

Ciao ragazzi! Se siete programmatori (o aspiranti tali), sicuramente avete già sentito parlare di testing del codice. Ma cosa significa esattamente? E perché è importante per i programmatori?

## Cosa & Perché?

Scrivere test è il processo di creare codice specifico che verifica se il nostro codice funziona correttamente. Questo ci aiuta a identificare eventuali errori o bug nel nostro codice prima che venga utilizzato dai nostri utenti. Inoltre, i test forniscono una documentazione utile sulle funzionalità del nostro codice e ci aiutano a mantenere la qualità del nostro software nel tempo.

## Come fare:

Per scrivere test in Rust, utilizziamo il modulo integrato di testing "test". Iniziamo creando una funzione di test utilizzando l'attributo `#[test]`. All'interno di questa funzione, utilizziamo l'assertion `assert_eq!()` per confrontare il risultato della nostra funzione con un risultato predefinito. Vediamo un esempio:

```Rust
#[test]
fn test_addition() {
    let result = add(5, 10);
    assert_eq!(result, 15);
}
```

Possiamo anche testare funzioni che ci aspettiamo che generino un errore utilizzando l'attributo `#[should_panic]`. Ad esempio:

```Rust
#[test]
#[should_panic]
fn test_division_by_zero() {
    div(10, 0);
}
```

Una volta scritte le nostre funzioni di test, possiamo eseguirle utilizzando il comando `cargo test` nella nostra cartella del progetto. Vedremo l'output dei test eseguiti e se sono tutti passati o meno.

## Approfondimento:

Scrivere test per il nostro codice prima era considerato una pratica non essenziale, ma negli ultimi anni è diventato sempre più importante. Grazie all'adozione di metodologie di sviluppo come il "Test Driven Development" (TDD) e "Continuous Integration" (CI), i test sono diventati un modo fondamentale per assicurare la qualità del nostro codice.

Oltre al modulo di testing integrato di Rust, esistono anche altri framework di testing come "JUnit" e "RSpec". Inoltre, ci sono anche strumenti di testing automatizzati che possono aiutarci a eseguire i nostri test in modo più efficiente.

Per quanto riguarda l'implementazione dei test, è importante ricordare di scrivere test indipendenti e non dipendenti da altri test. Inoltre, è consigliato seguire il principio di "write once, test everywhere" per assicurare che il nostro codice sia testato su più piattaforme.

## Vedi anche:

- [Documentazione sul modulo di testing di Rust](https://doc.rust-lang.org/stable/book/ch11-01-writing-tests.html)
- [Articolo su Test Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
- [Continous Integration: cos'è e perché è importante](https://en.wikipedia.org/wiki/Continuous_integration)