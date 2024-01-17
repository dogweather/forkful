---
title:                "Convertire una stringa in minuscolo"
html_title:           "Rust: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Con la conversione di una stringa in minuscolo si intende trasformare tutti i caratteri di una stringa in lettere minuscole. I programmatori spesso fanno questo perché alcune funzioni di ricerca o confronto richiedono una stringa in minuscolo per funzionare correttamente.

## Come fare:
```Rust
let stringa = "Progetto Rust";
let stringa_minuscola = stringa.to_lowercase();
println!("{}", stringa_minuscola);
```
Output: "progetto rust"

## Approfondimento:
(1) Questa pratica è nata negli anni '70 per semplificare la manipolazione dei dati e migliorare l'efficienza dei programmi scritti in linguaggi di programmazione come C. (2) Esistono diverse alternative per convertire una stringa in minuscolo, come l'utilizzo di funzioni o librerie esterne. (3) In Rust, la conversione di una stringa in minuscolo è implementata attraverso il metodo "to_lowercase()" del tipo "String".

## Vedi anche:
- Documentazione ufficiale di Rust sulla conversione tra tipi: https://doc.rust-lang.org/std/convert/trait.From.html
- Un articolo sui diversi metodi per convertire una stringa in minuscolo in Rust: https://www.rockyourcode.com/how-to-lowercase-a-string-in-rust/