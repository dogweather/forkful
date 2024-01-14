---
title:                "Rust: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Il calcolo della lunghezza di una stringa è una delle operazioni più comuni quando si lavora con stringhe in un linguaggio di programmazione. In Rust, ci sono diverse opzioni per trovare la lunghezza di una stringa, ognuna con le proprie peculiarità. In questo articolo, esploreremo come trovare la lunghezza di una stringa in Rust.

## Come fare

Per trovare la lunghezza di una stringa in Rust, possiamo utilizzare il metodo `len()` che fa parte del tipo di dato `String`. Questo metodo restituirà la lunghezza della stringa come un numero intero.

````Rust
fn main() {
    let stringa = "Ciao, mondo!";
    println!("La lunghezza della stringa è: {}", stringa.len());
}
````

Output:

```
La lunghezza della stringa è:  12
```

Se stiamo lavorando con una stringa di tipo `&str` invece che con una `String`, dobbiamo utilizzare il metodo `len_utf8()` per ottenere la lunghezza in termini di byte UTF-8. Questo perché Rust ha un sistema di codifica delle stringhe che considera i caratteri UTF-8 come byte individuali. Dobbiamo quindi utilizzare questo metodo per ottenere la lunghezza effettiva della stringa.

````Rust
fn main() {
    let stringa = "Ciao, mondo!";
    println!("La lunghezza della stringa è: {}", stringa.len_utf8());
}
````

Output:

```
La lunghezza della stringa è:  12
```

## Approfondimento

Sebbene il metodo `len()` sia la scelta più comune per trovare la lunghezza di una stringa in Rust, è importante ricordare che questo metodo restituisce la lunghezza della stringa in termini di byte, non di caratteri. Ciò significa che se la stringa contiene caratteri che richiedono più di un byte per essere rappresentati in UTF-8, la lunghezza restituita potrebbe non corrispondere al numero di caratteri presenti nella stringa.

Per risolvere questo problema, possiamo utilizzare il metodo `chars()` che restituisce un iteratore sui caratteri della stringa. Possiamo quindi utilizzare il metodo `count()` su questo iteratore per ottenere il numero effettivo di caratteri nella stringa.

````Rust
fn main() {
    let stringa = "Привет, мир!";
    println!("La lunghezza in caratteri della stringa è: {}", stringa.chars().count());
}
````

Output:

```
La lunghezza in caratteri della stringa è: 11
```

## Vedi anche

- [Documentazione ufficiale sull'utilizzo del metodo `len()`](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Documentazione ufficiale sull'utilizzo del metodo `len_utf8()`](https://doc.rust-lang.org/std/primitive.str.html#method.len_utf8)
- [Documentazione ufficiale sull'utilizzo del metodo `chars()`](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)
- [Documentazione ufficiale sull'utilizzo del metodo `count()`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.count)