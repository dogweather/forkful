---
title:                "Rust: Convertire una stringa in minuscolo."
simple_title:         "Convertire una stringa in minuscolo."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Perché

Se stai scrivendo un codice Rust che coinvolge stringhe, potresti voler convertire una stringa in minuscolo. Questo è particolarmente utile per confrontare due stringhe in modo case-insensitive o per stampare una stringa in un formato uniforme.

##Come Fare

Fortunatamente, esiste un metodo incorporato in Rust per eseguire questa operazione in modo semplice e veloce. Utilizzando il metodo `to_lowercase()` su una variabile di tipo `String`, è possibile convertire la stringa in minuscolo.

```
Rust
let stringa = String::from("Ciao! Questa STRINGA sarà convertita in minuscolo.");

println!("{}", stringa.to_lowercase());

// Output: ciao! questa stringa sarà convertita in minuscolo.
```

Il metodo `to_lowercase()` crea una nuova stringa in minuscolo, lasciando la stringa originale immutata. Inoltre, funziona non solo per stringhe italiane, ma per qualsiasi set di caratteri Unicode.

##Deep Dive

Se vuoi saperne di più su come funziona il metodo `to_lowercase()`, dovresti sapere che utilizza le tabelle di conversione Unicode per convertire ogni carattere in minuscolo. Questa operazione è molto veloce ed efficiente, specialmente rispetto ad altre lingue di programmazione.

Inoltre, è importante sottolineare che il metodo `to_lowercase()` è sicuro da usare, anche per caratteri "speciali" come accenti, punti diaritici o segni diacritici.

##Vedi Anche

- La documentazione ufficiale di Rust sul metodo `to_lowercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Un tutorial su come lavorare con stringhe in Rust: https://medium.com/@erikhallander/rust-tutorial-series-working-with-strings-4a4dd71a7e75
- Un esempio pratico di come utilizzare il metodo `to_lowercase()` in un progetto Rust: https://gist.github.com/kevinconroy/765d1eb8adf1d266596e32df638e6e9c