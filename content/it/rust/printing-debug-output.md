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

## Perché

Potresti chiederti perché dovresti preoccuparti di stampare l'output di debug nel tuo codice Rust. La risposta è semplice: la stampa di output di debug è un potente strumento di debugging che ti aiuta a identificare e risolvere rapidamente eventuali problemi nel tuo programma.

## Come fare

Per stampare l'output di debug nel tuo codice Rust, puoi utilizzare la funzione `println!()`. Questa funzione accetta come argomenti il formato di stampa e una lista di variabili che vuoi stampare. Ecco un esempio di codice:

```Rust
let name = "Marco";
let age = 25;
println!("Ciao, mi chiamo {} e ho {} anni.", name, age);
```

L'output sarà: `Ciao, mi chiamo Marco e ho 25 anni.`

Puoi anche utilizzare la macro `dbg!()` per stampare l'output di debug, che ti mostrerà anche il nome della variabile e il suo valore. Ecco un esempio:

```Rust
let x = 5;
let y = 10;
let product = x * y;
dbg!(product);
```

L'output sarà: `product: 50` 

## Approfondimento

Oltre alla funzione `println!()` e alla macro `dbg!()`, esistono altre opzioni per la stampa di output di debug in Rust. Puoi utilizzare la libreria `log` per gestire log di diversi livelli di gravità e controllare quale output viene visualizzato in base alle tue esigenze.

Inoltre, puoi utilizzare l'attributo `#[derive(Debug)]` per rendere stampabile in modo automatico una struttura o un'enumerazione usando `println!()` o `dbg!()`. Ecco un esempio:

```Rust
#[derive(Debug)]
enum Animal {
    Dog,
    Cat,
    Bird,
}

let my_pet = Animal::Dog;
dbg!(my_pet);
```

L'output sarà: `my_pet: Animal::Dog`

## Vedi anche

Se vuoi saperne di più sul debugging in Rust, puoi consultare questi utili link:

- [Debugging in Rust](https://doc.rust-lang.org/book/ch01-05-programming-a-guessing-game.html#programming-a-guessing-game)
- [Guide to Rust Debugging](https://medium.com/@samdutton/a-guide-to-rust-debugging-938f37299a87)
- [Logging in Rust with the Log Crate](https://www.jonathanturner.org/2015/03/logging-in-rust-with-the-log-crate.html)