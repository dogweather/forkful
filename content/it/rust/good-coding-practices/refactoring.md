---
title:                "Rifattorizzazione"
date:                  2024-01-26T03:36:40.692482-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/refactoring.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il **Refactoring** è il processo di ristrutturazione del codice informatico esistente—modificando la fattorizzazione—senza alterarne il comportamento esterno. I programmatori lo fanno per migliorare attributi non funzionali del software, come la leggibilità, la riduzione della complessità, migliorare la manutenibilità e creare un'architettura interna o un modello di oggetto più espressivo per migliorare l'estensibilità.

## Come fare:

Rifacciamo il refactoring di un semplice pezzo di codice Rust per renderlo più idiomatico e manutenibile. Iniziamo con una funzione che calcola la somma di un vettore di interi:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La somma è {}", sum(&numbers));
}
```

Output:
```
La somma è 15
```

Ora, rifacciamo il refactoring per usare Rust più idiomatico sfruttando gli iteratori e il metodo `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La somma è {}", sum(&numbers));
}
```

Nessun cambiamento nell'output—è ancora `15`—ma la versione refactorizzata è più pulita e sfrutta i punti di forza di Rust come il prestito e i metodi degli iteratori.

## Approfondimento

Il Refactoring ha le sue radici nella comunità di Smalltalk ed è stato reso popolare nel mondo Java dal libro di Martin Fowler "Refactoring: Improving the Design of Existing Code". I suoi principi sono universali e si applicano anche a Rust, dove la sicurezza e la concorrenza sono di fondamentale importanza. Rust incoraggia la scrittura di codice robusto intercettando i problemi al momento della compilazione, quindi durante il refactoring, il compilatore di Rust agisce come una rete di sicurezza.

Alternative al refactoring manuale includono l'uso di strumenti automatizzati, come 'rustfmt' per la formattazione del codice e 'clippy' per il linting, che possono suggerire modi più idiomatici di scrivere codice. Tuttavia, il refactoring approfondito spesso richiede una comprensione riflessiva del design del codice, che questi strumenti non possono automatizzare completamente.

In Rust, il refactoring potrebbe ruotare attorno al miglioramento dell'uso dei tipi, sfruttando efficacemente i lifetimes, riducendo le allocazioni non necessarie o impiegando modelli di concorrenza come l'uso di `Arc<Mutex<T>>` quando necessario. È anche comune passare da `unwrap()` a una gestione degli errori più espressiva con `Result<T, E>`.

## Vedi Anche

Per approfondire il refactoring in Rust:

- Il libro di Rust: https://doc.rust-lang.org/book/
- Rust per Esempio: https://doc.rust-lang.org/rust-by-example/
- Clippy, uno strumento di linting per Rust: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" di Martin Fowler: https://martinfowler.com/books/refactoring.html
