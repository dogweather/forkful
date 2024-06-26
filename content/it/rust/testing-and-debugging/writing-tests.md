---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:53.873335-07:00
description: "Come farlo: Il framework di test integrato in Rust supporta test di\
  \ unit\xE0, integrazione e documentazione senza la necessit\xE0 di librerie esterne.\
  \ I test\u2026"
lastmod: '2024-03-13T22:44:43.221528-06:00'
model: gpt-4-0125-preview
summary: "Il framework di test integrato in Rust supporta test di unit\xE0, integrazione\
  \ e documentazione senza la necessit\xE0 di librerie esterne."
title: Scrivere test
weight: 36
---

## Come farlo:
Il framework di test integrato in Rust supporta test di unità, integrazione e documentazione senza la necessità di librerie esterne. I test sono annotati con `#[test]`, e ogni funzione così annotata viene compilata come un test.

### Scrivere un Test di Unità:
Posiziona i test di unità nel modulo che stai testando utilizzando un sotto-modulo `tests` marcato con `#[cfg(test)]` per garantire che siano compilati solo durante il testing.

```rust
// lib.rs o main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Esecuzione dei test:
```shell
$ cargo test
```

Output:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (o src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Scrivere Test di Integrazione:
I test di integrazione vanno in una directory `tests` al livello più alto del tuo progetto, accanto a `src`. Ogni file `.rs` in `tests` viene compilato come crate separato.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testing con Librerie di Terze Parti Popolari:
Per capacità di testing più estese, la libreria `proptest` può generare un'ampia gamma di input per testare le funzioni.

Aggiungi `proptest` come dipendenza di sviluppo in `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Usa `proptest` per eseguire lo stesso test con molti input generati automaticamente:

```rust
// dentro tests/integration_test.rs o un modulo #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Questo verifica che `add` non vada in panic per una vasta gamma di input `i32`.
