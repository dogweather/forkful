---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
L'interpolazione delle stringhe in Rust permette di inserire valore di variabili o espressioni direttamente in una stringa. Facilita notevolmente la formattazione e la visualizzazione dei dati nel codice.

## Come Fare:
In Rust, l'interpolazione delle stringhe si realizza mediante un macro chiamato `format!`. Ecco un esempio:
```Rust
let nome = "Mondo";
let saluto = format!("Ciao, {}", nome);
println!("{}", saluto);
```
Nel codice qui sopra, la variabile `nome` viene inserita nella stringa all'atto dell'esecuzione del programma, producendo il seguente output:

`Ciao, Mondo`

## Approfondimento
La caratteristica di interpolazione delle stringhe ha una lunga storia nel mondo della programmazione, con antenati che risalgono alle prime versioni di Perl e Ruby. Rust ha scelto di implementare l'interpolazione utilizzando un macro, che offre più controllo e sicurezza rispetto ai metodi di interpolazione incorporati in altri linguaggi.

Un'alternativa all'interpolazione delle stringhe in Rust potrebbe essere l'utilizzo del metodo `push_str()` o `+` per concatenare le stringhe, ma questi metodi possono essere più lenti e meno leggibili.

Dal punto di vista dell'implementazione, la macro `format!` in Rust crea una forma di RFC 5424 che viene poi compilata nel codice macchina.

## Vedi Anche
- Rust Documentation: [std::format!](https://doc.rust-lang.org/stable/std/macro.format.html)
- Rust By Example: [String Formatting](https://doc.rust-lang.org/stable/rust-by-example/hello/print/fmt.html)
- Stack Overflow: [How do I concatenate strings?](https://stackoverflow.com/questions/30186037/how-do-i-concatenate-strings)