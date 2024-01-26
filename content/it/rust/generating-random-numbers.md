---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:51.241313-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è lo scopo di creare valori non predittibili per usi vari quali giochi, simulazioni e criptografia. I programmatori lo fanno per infondere un elemento di casualità e imprevedibilità nelle loro applicazioni.

## How to:
Per generare numeri casuali in Rust, si usa il crate `rand`. Ecco un esempio semplice.

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let num: i32 = rng.gen_range(0..10);
    println!("Numero casuale: {}", num);
}
```

Output di esempio:
```
Numero casuale: 4
```

## Deep Dive
La generazione di numeri casuali ha una storia interessante: dai semplici strumenti meccanici ai complessi algoritmi computazionali. In Rust, `rand` è la scelta principale, ma ci sono alternative come `fastrand` o `oorandom` per diverse esigenze. Il crate `rand` offre diversi generatori di numeri casuali (PRNGs), con vari livelli di velocità, sicurezza e compatibilità cross-platform.

## See Also
- Documentazione del crate `rand`: https://docs.rs/rand
- Una discussione su alternative PRNG: https://users.rust-lang.org/t/crate-evaluation-for-2017-07-25-oorandom/12006
- Libro ufficiale di Rust, capitolo sulla generazione di numeri casuali: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
