---
title:                "Utilizzando le espressioni regolari"
html_title:           "Rust: Utilizzando le espressioni regolari"
simple_title:         "Utilizzando le espressioni regolari"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è e perché usarlo?
Le espressioni regolari sono un modo per ricercare e manipolare testi e stringhe di caratteri basato su pattern specifici. I programmatori le utilizzano per facilitare le operazioni di ricerca o sostituzione di parole o caratteri in un grande quantitativo di testo.

## Come usarlo:
```Rust
// Esempio di espressione regolare per trovare tutte le parole che iniziano con "c" e terminano con "t"
use regex::Regex;

let re = Regex::new(r"c\w?t").unwrap();

let testo = "Questo è un testo di prova che contiene parole come carota e compito.";

for corrispondenza in re.find_iter(testo) {
    println!("Parola corrispondente: {}", corrispondenza.as_str());
}

// Output: Parola corrispondente: carota
//         Parola corrispondente: compito
```

## Approfondimento:
Le espressioni regolari sono state sviluppate negli anni '40 da matematici e linguisti come strumento per la ricerca di informazioni all'interno di testi. Oggi sono utilizzate in molti linguaggi di programmazione, come Java e Python, e anche in programmi di editing di testo come Vim.

Un'alternativa alle espressioni regolari sono i parser, che sono più potenti e flessibili ma richiedono maggiore conoscenza e tempo per essere implementati correttamente.

In Rust, le espressioni regolari sono implementate tramite la crate regex, che utilizza il motore di ricerca adattativo di Thompson. Questo motore ha una buona velocità e utilizza un algoritmo non deterministico, rendendo le espressioni regolari più semplici da utilizzare rispetto ad altri motori.

## Vedi anche: