---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Generare numeri casuali si riferisce al processo di creazione di numeri in modo casuale, senza un ordine o un modello prevedibile. Questa funzionalità è essenziale nella programmazione perché viene utilizzata in molteplici applicazioni come la crittografia, la simulazione e i giochi.

## Come fare:
Utilizzaremo la libreria `rand` di Rust per generare i numeri casuali. Ecco come farlo:

```Rust
// Aggiungere la libreria
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    println!("Numero casuale tra 0 e 10: {}", rng.gen_range(0..10));
}
```

Quando esegui il codice, vedrai un output come questo:

```Rust
"Numero casuale tra 0 e 10: 7"
```

## Approfondimenti
Historicamente, generare numeri casuali in modo efficace è stato una sfida per i programmatori. Rust risolve questo problema utilizzando un generatore di numeri pseudocasuali chiamato "thread_rng", che genera numeri casuali ad alta velocità e alta qualità.

In termini di alternative, esistono altre funzioni in Rust per la generazione di numeri casuali, come `gen()` e `gen_bool()`. 

A livello di implementazione, `rand` utilizza un tipo di generatore di numeri casuali che si basa su un metodo matematico chiamato "xorshift" per generare numeri casuali in modo efficiente e sicuro.

## Vedi Anche
Per ulteriori informazioni su come generare numeri casuali in Rust, puoi consultare i seguenti link:

- Documentazione Ufficiale Rust: [Rust Official Documentation](https://doc.rust-lang.org/rand/rand/index.html)
- Thread su StackOverflow: [How to generate a random number in Rust](https://stackoverflow.com/questions/36821263/how-to-generate-a-random-number-in-rust)