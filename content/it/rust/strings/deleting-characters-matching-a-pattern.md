---
date: 2024-01-20 17:43:10.809689-07:00
description: 'Come fare: Un esempio semplice per editar stringhe elimina i caratteri
  non desiderati.'
lastmod: '2024-04-05T21:53:43.975172-06:00'
model: gpt-4-1106-preview
summary: Un esempio semplice per editar stringhe elimina i caratteri non desiderati.
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## Come fare:
```rust
fn rimuovi_caratteri<S: AsRef<str>>(testo: S, pattern: S) -> String {
    testo.as_ref().chars().filter(|c| !pattern.as_ref().contains(*c)).collect()
}

fn main() {
    let frase = "Ciao, Rustacei! È ora di codare.";
    let caratteri_da_rimuovere = ",!.";
    let frase_pulita = rimuovi_caratteri(&frase, &caratteri_da_rimuovere);

    println!("Frase originale: {}", frase);
    println!("Frase pulita: {}", frase_pulita);
}

// Output
// Frase originale: Ciao, Rustacei! È ora di codare.
// Frase pulita: Ciao Rustacei È ora di codare
```
Un esempio semplice per editar stringhe elimina i caratteri non desiderati.

## Approfondimento
Prima che il linguaggio Rust risultasse stabile, le operazioni sulle stringhe potevano essere più complicate. La comunità ha lavorato duro per sviluppare metodi ergonomici e performanti come `filter` e `collect`. 

Alternativamente, esiste la crate `regex` che offre funzionalità potenti per manipolazione basata su espressioni regolari. Tuttavia, è più pesante e indirizzata a casi più complessi.

Il dettaglio d'implementazione chiave qui è l'utilizzo di iterators e closure per fornire un metodo funzionale, efficiente ed espressivo per lavorare con stringhe.

## Vedi anche
- [Rust Documentation: std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
- [The Rust Programming Language Book](https://doc.rust-lang.org/stable/book/)
