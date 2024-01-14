---
title:    "Rust: Generazione di numeri casuali"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è un aspetto fondamentale della programmazione, in quanto può essere utilizzato per la creazione di giochi o per il testing di algoritmi.

## Come Fare
Per generare numeri casuali in Rust, è possibile utilizzare la libreria standard "rand". Inizialmente, dobbiamo aggiungere la dipendenza nel nostro file `Cargo.toml`:

```Rust
[dependencies]
rand = "0.8.0"
```

Successivamente, possiamo utilizzare la funzione `thread_rng()` per inizializzare il nostro generatore di numeri casuali e la funzione `gen_range()` per generare un numero all'interno di un determinato range.

Esempio di codice:

```Rust
use rand::prelude::*;

fn main() {
    let mut rng = thread_rng();
    let random_number = rng.gen_range(1..=10);
    println!("Il numero casuale è: {}", random_number);
}
```

Output:

```
Il numero casuale è: 5
```

## Approfondimento
La libreria "rand" offre diverse funzionalità per la generazione di numeri casuali, come ad esempio la possibilità di scegliere una distribuzione specifica (uniforme, normale, etc.), di generare numeri a virgola mobile e di utilizzare seed personalizzati.

Inoltre, è importante tenere in considerazione che la generazione di numeri casuali non è veramente casuale, ma è basata su un algoritmo. Pertanto, è importante scegliere un buon generatore di numeri casuali e utilizzarlo in modo corretto per evitare di influenzare la casualità dei numeri generati.

## Vedi Anche
- Documentazione ufficiale della libreria "rand": https://docs.rs/rand/0.8.0/rand/
- Tutorial su generazione di numeri casuali in Rust: https://www.educative.io/edpresso/generating-random-numbers-in-rust
- Tutorial su come scegliere un buon generatore di numeri casuali: https://programming.guide/prng-random-number-generator.html