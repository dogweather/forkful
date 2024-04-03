---
date: 2024-01-20 17:48:16.922077-07:00
description: "How to: In Rust, ottenere la lunghezza di una stringa \xE8 semplice\
  \ grazie al metodo `.len()`."
lastmod: '2024-03-13T22:44:43.209040-06:00'
model: gpt-4-1106-preview
summary: "In Rust, ottenere la lunghezza di una stringa \xE8 semplice grazie al metodo\
  \ `.len()`."
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to:
In Rust, ottenere la lunghezza di una stringa è semplice grazie al metodo `.len()`:

```Rust
fn main() {
    let saluto = "Ciao, mondo!";
    let lunghezza = saluto.len();
    println!("La lunghezza della stringa è: {}", lunghezza);
}
```

Output:
```
La lunghezza della stringa è: 13
```

## Deep Dive
In Rust, le stringhe sono codificate in UTF-8. Il metodo `.len()` restituisce il numero di byte, non di caratteri. È importante perché alcuni caratteri usano più di un byte.

Storicamente, altri linguaggi potrebbero usare la lunghezza in caratteri, ma con UTF-8, Rust sceglie byte per prestazioni e sicurezza.

Se hai bisogno di contare i caratteri effettivi, usa `.chars().count()`:

```Rust
fn main() {
    let saluto = "Ciao, mondo!";
    let caratteri = saluto.chars().count();
    println!("Il numero di caratteri è: {}", caratteri);
}
```

Output:
```
Il numero di caratteri è: 13
```

Ma attento se hai caratteri Unicode come emoji: 🦀 (granchio) o 😊 (sorriso) sono codificati in più byte.

## See Also
- [La documentazione ufficiale di Rust sulle stringhe](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [UTF-8 encoding](https://en.wikipedia.org/wiki/UTF-8)
- [API Rust per String](https://doc.rust-lang.org/std/string/struct.String.html)
