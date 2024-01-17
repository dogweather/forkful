---
title:                "Stampa dell'output di debug"
html_title:           "Rust: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Stampare output di debug è un modo per verificare e rivedere il codice mentre eseguiamo il programma. Per esempio, possiamo stampare il valore di una variabile o una frase, per verificare se sono corretti. I programmatori spesso lo fanno per capire come il codice sta funzionando e dove sono presenti eventuali errori.

## Come fare:
In Rust, possiamo stampare output di debug utilizzando la macro "println!". Possiamo inserirla nel codice dove vogliamo che l'output venga visualizzato. Di seguito un esempio:

```Rust
let x = 10;
println!("Il valore di x è {}", x);

// Output: Il valore di x è 10
```

Possiamo anche stampare più variabili o stringhe nella stessa macro, separandole con una virgola:

```Rust
let nome = "Mario";
let cognome = "Rossi";
println!("Il mio nome è {}, il mio cognome è {}", nome, cognome);

// Output: Il mio nome è Mario, il mio cognome è Rossi
```

## Approfondimento:
Stampare output di debug è una pratica comune tra i programmatori da molto tempo. In Rust, possiamo anche utilizzare altre macro come "dbg!" o "eprintln!", che gestiscono in modo diverso l'output di debug. Possiamo anche utilizzare un debugger, un programma che ci permette di eseguire il codice passo dopo passo e monitorare le variabili e gli output di debug.

## Vedi anche:
- Documentazione ufficiale di Rust: https://doc.rust-lang.org/std/fmt/#macros
- Tutorial su come utilizzare il debugger GDB in Rust: https://www.linuxjournal.com/content/debugging-rust-using-gdb
- Un esempio di utilizzo delle macro di debug in Rust: https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging/print/debugging-output.html