---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Rust: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Perché dovresti essere interessato a trovare la lunghezza di una stringa? Beh, è importante per molte operazioni di manipolazione delle stringhe come la ricerca, l'elaborazione e la formattazione.

## Come fare
Ecco come puoi trovare la lunghezza di una stringa in Rust utilizzando il metodo `len()`:

```Rust
let stringa = "Ciao mondo";
let lunghezza = stringa.len();

println!("La lunghezza della stringa è: {}", lunghezza);
```

Output:

```
La lunghezza della stringa è: 10
```

Il metodo `len()` restituisce il numero di byte presenti nella stringa, poiché Rust utilizza gli Unicode scalar values per rappresentare i caratteri. In questo esempio, ci sono 10 caratteri in "Ciao mondo", ma poiché alcuni caratteri possono essere rappresentati con più di un byte, la lunghezza è 10 e non 9.

Inoltre, puoi utilizzare il metodo `chars()` per ottenere il numero di caratteri invece del numero di byte:

```Rust
let stringa = "Ciao mondo";
let caratteri = stringa.chars().count();

println!("Il numero di caratteri nella stringa è: {}", caratteri);
```

Output:

```
Il numero di caratteri nella stringa è: 9
```

Questo è utile quando si lavora con stringhe multibyte come quelle contenenti caratteri Unicode.

## Approfondimento
Ora che sai come ottenere la lunghezza di una stringa, potresti chiederti come funziona il metodo `len()` e perché ci sono due modi per ottenerlo. In effetti, il metodo `len()` è chiamato sulla struttura dati `String` di Rust, che è un wrapper intorno al tipo di base `Vec<u8>` (vettore di byte). Questo significa che quando richiami il metodo `len()` su una stringa, stai accedendo alla sua proprietà `len` che rappresenta il numero di elementi nel vettore.

Inoltre, il metodo `count()` utilizzato nel secondo esempio è fornito dal trait `Chars` e viene implementato da `String`. Questo metodo restituisce il numero di caratteri in una stringa indipendentemente dal numero di byte utilizzati per rappresentarli.

## Vedi anche
- [La documentazione della libreria standard di Rust sull'uso delle stringhe](https://doc.rust-lang.org/std/string/index.html)
- [Un tutorial su come manipolare le stringhe in Rust](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [Una guida su come utilizzare i byte e i caratteri in Rust](https://www.brianl.dev/blog/rust-bytes-and-chars/)