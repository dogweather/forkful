---
title:                "Rust: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molti motivi per cui potresti voler generare numeri casuali in un programma Rust. Forse stai sviluppando un gioco e hai bisogno di una funzione per generare posizioni casuali o valori di vite per i giocatori. O forse stai lavorando su un algoritmo di machine learning che richiede dati casuali per l'addestramento. In ogni caso, generare numeri casuali è un'abilità molto utile da avere quando si programma in Rust.

## Come fare
Per generare numeri casuali in Rust, può essere utilizzata la libreria standard `std::rand`. Includila nel tuo progetto aggiungendo questa linea all'inizio del tuo file Rust:

```Rust
use std::rand;
```

Una volta inclusa la libreria, puoi utilizzare la funzione `rand::random()` per generare un numero intero casuale.

```Rust
let random_number: i32 = rand::random();
```

Se vuoi specificare un range di numeri tra cui generare, puoi utilizzare il metodo `gen_range()`:

```Rust
let random_number: i32 = rand::thread_rng().gen_range(1, 100);
```

Questo genererà un numero intero casuale compreso tra 1 e 100. Assicurati di importare `rand::Rng` per poter utilizzare il metodo `thread_rng()`.

```Rust
use std::rand::Rng;
```

È anche possibile generare numeri casuali di altri tipi, come `f32` o `char`. Per ulteriori informazioni su come specificare un tipo di numero casuale, consulta la documentazione della libreria `std::rand`.

## Approfondimento
La generazione di numeri casuali è una tecnica molto importante in programmazione e può essere utilizzata in molti contesti diversi. Ma è anche un problema complesso. La libreria `std::rand` utilizza un algoritmo basato su XORShift, che è considerato molto veloce ma non completamente casuale. Inoltre, la generazione di numeri casuali può comportare alcune difficoltà quando si tratta di testare il codice.

Se vuoi saperne di più su come funziona la generazione di numeri casuali e sui diversi algoritmi utilizzati, ti consiglio di leggere la documentazione della libreria `rand` e di fare ulteriori ricerche sulla teoria e la pratica della generazione di numeri casuali.

## Vedi anche
- [Documentazione della libreria std::rand](https://doc.rust-lang.org/std/rand/)
- [Articolo sulla teoria della generazione di numeri casuali in informatica](https://en.wikipedia.org/wiki/Random_number_generation)
- [Codice di esempio per la generazione di numeri casuali in Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=8919eff6800e88c3983770979e8bd1ea)