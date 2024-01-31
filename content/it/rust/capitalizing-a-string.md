---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutte le lettere minuscole in lettere maiuscole. I programmatori lo fanno per standardizzare l'input di testo e migliorare la leggibilità o per scopi specifici di formattazione.

## How to:

Rust rende il processo piuttosto diretto con il metodo `to_uppercase()` disponibile sulle stringhe:

```Rust
fn main() {
    let saluto = "ciao mondo!";
    let saluto_maiuscolo = saluto.to_uppercase();

    println!("{}", saluto_maiuscolo); // Output: CIAO MONDO!
}
```

Questo esempio stampa `CIAO MONDO!`, mostrando la stringa originale totalmente in maiuscolo.

## Deep Dive

Rust gestisce la capitalizzazione rispettando la specifica Unicode per la conversione in maiuscolo, il che significa che funziona oltre i semplici alfabeti ASCII. Storicamente, altre lingue di programmazione hanno avuto approcci più limitati, considerando solo i caratteri inglesi.

Un'alternativa all'uso di `to_uppercase()` è l'iterazione manuale sui caratteri di una stringa e la loro conversione singola con metodi come `to_ascii_uppercase`, ma questo non è raccomandato per i caratteri non ASCII.

A livello di implementazione, il metodo `to_uppercase()` può allocare più memoria se la versione maiuscola di una stringa richiede più spazio dei caratteri originali, un caso comune con alcuni caratteri Unicode.

## See Also

Per una comprensione più completa delle stringhe in Rust e delle loro funzionalità, assicurati di consultare la documentazione ufficiale:

- Rust String Docs: https://doc.rust-lang.org/std/string/struct.String.html
- Rust documentation on `to_uppercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Unicode Consortium per approfondire le specifiche Unicode: https://unicode.org
