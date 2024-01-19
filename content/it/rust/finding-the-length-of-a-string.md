---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Trovare la lunghezza di una stringa significa determinare il numero di caratteri in essa. I programmatori lo fanno per vari motivi, ad esempio per ciclare all'interno della stringa, validare l'input, ecc.

## Come fare:

Ecco un esempio semplice su come ottenere la lunghezza di una stringa in Rust.

```Rust
fn main() {
    let s = "Ciao Mundo!";
    println!("{}", s.len());
}
```

All'esecuzione di questo codice, stampa `12`, che è la lunghezza della stringa `Ciao Mundo!`.

Se hai una stringa con caratteri Unicode, il metodo `len()` potrebbe non comportarsi come ti aspetti. In questo caso, puoi utilizzare il metodo `chars().count()`, come mostrato di seguito.

```Rust
fn main() {
    let s = "Ciao Mündò!";
    println!("{}", s.chars().count());
}
```
Eseguendo questo codice otterai `11`, tenendo conto dei caratteri Unicode.

## Approfondimenti:

Nel contesto storico, Rust ha introdotto il metodo `len()` con l'intenzione di fornire un modo semplice e rapido per ottenere il conteggio dei byte di una stringa. È importante notare che `len()` ritorna la lunghezza in byte della String, non il conteggio dei caratteri Unicode.

In alternativa, per ottenere il conteggio dei caratteri Scala Unicode, puoi usare il metodo `chars().count()`. Rust implementa la libreria standard `str` che fornisce vari metodi tra cui `len()` e `chars().count()` che aiutano a determinare la lunghezza di una stringa.

## Vedi Anche:

[Rust Documentation - len()](https://doc.rust-lang.org/std/string/struct.String.html#method.len)

[Rust Documentation - chars()](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)

[Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)

[Rust Internals - What's the deal with Strings in Rust?](https://internals.rust-lang.org/t/whats-the-deal-with-strings-in-rust/9812)