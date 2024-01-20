---
title:                "Stampa dell'output di debug"
html_title:           "Bash: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La stampa del debug output è un modo per visualizzare i dati all'interno del codice durante l'esecuzione. I programmatori la utilizzano per capire come il loro codice si comporta al fine di identificare e correggere gli errori.

## Come fare:

Ecco un semplice esempio di come stampare il debug output in Rust:

```Rust
fn main() {
    let s = String::from("Salve, mondo!");
    println!("{:?}", s);
}
```

Ecco cosa viene visualizzato:

```Rust
"Salve, mondo!"
```

## Approfondimento

Historicamente, la stampa del debug output è stata una delle tecniche più antiche utilizzate dai programmatori per il debugging. In Rust, è possibile utilizzare la macro `println!` per stampare il debug output, ma esistono anche alternative come `dbg!` e `Debug`.

Il modulo `Debug` rende facile stampare strutture di dati complesse in Rust. Ad esempio:

```Rust
fn main() {
    let v = vec![1, 2, 3];
    println!("{:?}", v);
}
```

Visualizzerà:

```Rust
[1, 2, 3]
```

## Vedi anche

Per ulteriori informazioni sulla stampa del debug output in Rust, consulta le seguenti risorse:

- [Documentazione ufficiale Rust](https://doc.rust-lang.org/book/ch18-01-all-the-places-for-panic.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- [Stack Overflow](https://stackoverflow.com/questions/27582739/how-do-i-print-a-rust-variable-for-debugging)