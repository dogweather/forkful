---
title:                "Trova la lunghezza di una stringa."
html_title:           "Rust: Trova la lunghezza di una stringa."
simple_title:         "Trova la lunghezza di una stringa."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Trovare la lunghezza di una stringa è una pratica comune tra i programmatori di Rust. Consiste nel determinare il numero di caratteri presenti in una stringa e può essere utile per diverse ragioni: ad esempio, per verificare l'integrità dei dati o per effettuare operazioni su porzioni specifiche della stringa.

## Come fare:

Per trovare la lunghezza di una stringa in Rust, possiamo utilizzare il metodo `len()` che restituisce il numero di byte presenti nella stringa. Ad esempio:

```Rust
let stringa = "Ciao, mondo!";
let lunghezza = stringa.len();
println!("La stringa \"{}\" ha una lunghezza di {} byte.", stringa, lunghezza);

// Output: "La stringa "Ciao, mondo!" ha una lunghezza di 13 byte."
```

Possiamo anche utilizzare il metodo `chars()` per ottenere il numero di caratteri invece dei byte. Ad esempio:

```Rust
let stringa = "Ciao, mondo!";
let lunghezza = stringa.chars().count();
println!("La stringa \"{}\" ha una lunghezza di {} caratteri.", stringa, lunghezza);

// Output: "La stringa "Ciao, mondo!" ha una lunghezza di 12 caratteri."
```

## Approfondimento:

La necessità di trovare la lunghezza di una stringa risale ai primi giorni della programmazione, quando le stringhe venivano memorizzate come array di caratteri e per determinarne la lunghezza era necessario scorrere tutta la stringa.

In Rust, oltre ai metodi `len()` e `chars()`, esistono anche altre alternative per trovare la lunghezza di una stringa. Ad esempio, possiamo utilizzare il metodo `as_bytes()` per ottenere l'array di byte della stringa e quindi utilizzare il metodo `len()` per trovare la sua lunghezza.

L'implementazione di `len()` in Rust è molto efficiente, in quanto tiene traccia della lunghezza della stringa in modo da non doverla calcolare ogni volta che viene richiamato il metodo.

## Vedi anche:

- [Rust string methods](https://doc.rust-lang.org/std/string/struct.String.html#methods)
- [Rust byte methods](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust str methods](https://doc.rust-lang.org/std/primitive.bytes.html)