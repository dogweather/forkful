---
title:                "Interpolare una stringa"
html_title:           "Rust: Interpolare una stringa"
simple_title:         "Interpolare una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perche?

L'interpolazione di stringhe è un modo per combinare diverse stringhe o variabili in un'unica stringa più grande. Questo è utile per creare output dinamico e personalizzato in base alle informazioni che è necessario visualizzare. I programmatori spesso usano l'interpolazione di stringhe per creare output più leggibili e organizzati.

## Come:

```Rust
let nome = "Giorgio";
let eta = 25;

let messaggio = format!("Ciao, mi chiamo {} e ho {} anni.", nome, eta);

println!("{}", messaggio);
```

Output:
```
Ciao, mi chiamo Giorgio e ho 25 anni.
```

## Approfondimento:

L'interpolazione di stringhe è stata introdotta in Rust nel 1.26 ed è stata una funzionalità molto richiesta dalla comunità di sviluppatori. Alcune alternative per la creazione di stringhe dinamiche includono la concatenazione di stringhe con l'operatore `+` o l'utilizzo della macro `format!`. Tuttavia, l'interpolazione di stringhe è considerata una soluzione più semplice e leggibile.

Per quanto riguarda l'implementazione, Rust utilizza formatter sottostanti come `fmt::Display` e `fmt::Debug` per generare la stringa interpolata. Inoltre, è possibile utilizzare espressioni all'interno delle parentesi `{}` per ottenere un output più complesso.

## Vedi anche:

- [The Rust Programming Language - String Interpolation](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)
- [Rust by Example - Printing](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)