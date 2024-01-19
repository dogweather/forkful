---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cosa e Perché?
La concatenazione delle stringhe è un'operazione comune nel mondo della programmazione che unisce due o più stringhe in una sola. Questo è indispensabile quando si desidera combinare dati di testo da diverse sorgenti o formati.

## Come fare:
Per concatenare le stringhe in Rust, c'è `.push_str()` e `+ &`. Ecco un esempio della concatenazione delle stringhe usando entrambi i metodi:

```Rust
fn main() {
    let saluto = String::from("Ciao ");
    let nome = String::from("Mario");
      
    // con push_str()
    let messaggio = saluto.clone();
    messaggio.push_str(&nome);
    println!("{}", messaggio); // Stampa: Ciao Mario

    // con + &
    let altro_messaggio = saluto + &nome;
    println!("{}", altro_messaggio); // Stampa: Ciao Mario
}
```

## Deep Dive:
La concatenazione delle stringhe risale all'inizio della programmazione. E' sempre stata una necessità fondamentale, ma la sua implementazione può variare a seconda del linguaggio.

In Rust, alternativamente a `.push_str()` e `+ &`, si può usare `format!()`. Questo può essere più leggibile quando si combinano molte stringhe o quando si includono variabili non stringhe.

```Rust
let messaggio = format!("{}{}", saluto, nome); 
```
Notate che con `+ &` la prima stringa viene consumata e non può essere riutilizzata dopo l'operazione.

## Vedere anche:
Per ulteriori informazioni sulla concatenazione delle stringhe in Rust, consultare le seguenti risorse:
1. Documentazione ufficiale Rust: [https://doc.rust-lang.org/stable/rust-by-example/std/str.html]
2. Stack Overflow post sulla concatenazione di stringhe in Rust: [https://stackoverflow.com/questions/30154541/how-to-concatenate-strings-in-rust]