---
title:                "Rust: Trova la lunghezza di una stringa"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Il trovare la lunghezza di una stringa è un'operazione comune quando si lavora con testi e dati di input. In questo articolo, esploreremo come farlo utilizzando Rust e perché questo linguaggio può essere un'ottima scelta per questo tipo di task.

## Come Farlo

Per trovare la lunghezza di una stringa in Rust, possiamo utilizzare il metodo `len()`. Questo metodo restituirà il numero di byte presenti nella stringa, che è anche equivalente alla sua lunghezza.

```
Rust
let my_string = "Ciao, sono una stringa!";
let lunghezza = my_string.len();

println!("La lunghezza della stringa è {}", lunghezza);

```

Output:
`La lunghezza della stringa è 24`

Inoltre, possiamo utilizzare anche il metodo `chars()` per ottenere il numero di caratteri effettivi nella stringa, invece che il numero di byte.

```
Rust
let my_string = "Ciao, sono una stringa!";
let lunghezza = my_string.chars().count();

println!("La lunghezza della stringa è {}", lunghezza);

```

Output:
`La lunghezza della stringa è 20`

Questo può essere utile se si lavora con stringhe contenenti caratteri multibyte.

## Deep Dive

In Rust, le stringhe vengono gestite in modo diverso rispetto ad altri linguaggi. Invece di utilizzare un array di caratteri, le stringhe sono immutabili e vengono memorizzate come slice di byte. Ciò rende l'operazione di trovare la lunghezza della stringa efficiente e veloce, in quanto non è necessario scorrere l'intera stringa per contare i caratteri.

Inoltre, Rust fornisce anche metodi aggiuntivi come `is_empty()` per verificare se una stringa è vuota e `contains()` per cercare una sottostringa all'interno della stringa principale.

## Vedi Anche

- [La documentazione ufficiale di Rust su stringhe](https://doc.rust-lang.org/std/string/)
- [Un tutorial su stringhe in Rust](https://tutorialedge.net/rust/the-rust-string-cheatsheet/)
- [Un altro articolo su come trovare la lunghezza di una stringa in Rust](https://www.techiediaries.com/string-length-rust/)