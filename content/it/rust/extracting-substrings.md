---
title:                "Estrazione di sottostringhe"
html_title:           "Rust: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Estrarre sottostringhe significa ottenere una parte specifica di una stringa più grande. I programmatori spesso lo fanno per accedere a informazioni specifiche all'interno di una stringa senza doverne copiare l'intero contenuto.

## Come fare:

Un esempio di base di come estrarre una sottostringa in Rust:

```
let stringa = "Ciao, mondo!";
let sottostringa = &stringa[5..10];
println!("{}", sottostringa);
```

Questo codice restituirà "mondo" poiché stiamo estraendo i caratteri dall'indice 5 all'indice 10 (escluso). Il codice utilizza una sintassi speciale di Rust per indicare gli indici della stringa. Si può anche specificare un solo indice per estrarre dalla posizione indicata fino alla fine della stringa.

## Approfondimento:

Per molti anni, l'estrazione di sottostringhe è stata una pratica comune nei linguaggi di programmazione. Tuttavia, con l'avanzamento della tecnologia, ci sono state nuove tecniche introdotte per gestire le stringhe, come le mutable strings e le strisce UTF-8. In Rust, le stringhe sono codificate in UTF-8 per impostazione predefinita per essere più efficienti e sicure.

Ci sono modi alternativi per estrarre sottostringhe in Rust, come utilizzare librerie esterne o moduli di espressioni regolari, che possono aiutare a svolgere l'attività in modo più efficiente. Inoltre, ci sono diversi metodi forniti dalla libreria standard di Rust per manipolare stringhe, compresa l'estrazione di sottostringhe.

Per quanto riguarda l'implementazione dell'estrazione di sottostringhe in Rust, viene utilizzata una tecnica chiamata "slicing". Questa tecnica consente di navigare attraverso gli elementi di una stringa senza crearne o copiarne una nuova. Ciò rende l'esecuzione dell'operazione più efficiente e meno costosa dal punto di vista della memoria.

## Vedi anche:

- [Documentazione per slice in Rust](https://doc.rust-lang.org/std/primitive.slice.html)
- [Tutorial sulle espressioni regolari in Rust](https://doc.rust-lang.org/std/primitive.slice.html)