---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutti i suoi caratteri in lettere maiuscole. I programmatori lo fanno per uniformare l'output dei testi, per esempio nei titoli o quando si confrontano stringhe senza tenere conto della loro capitalizzazione.

## How to:
In Gleam, capitalizzare una stringa è semplice. Ecco un esempio con l'output corrispondente:

```gleam
import gleam/string

pub fn main() {
  let my_string = "ciao, mondo!"
  let capitalised_string = string.to_uppercase(my_string)
  capitalised_string
}
```

Output:

```
"CIAO, MONDO!"
```

## Deep Dive
Historicamente, la capitalizzazione è servita per enfatizzare parole o frasi e nella programmazione non è diversa. La capitalizzazione può aiutare nell'ordinamento alfabetico o nel confronto di stringhe senza distinzione tra maiuscole e minuscole.

Alternative all'uso della funzione `string.to_uppercase` potrebbero includere l'implementazione di una tua funzione personalizzata tramite la mappatura di ogni carattere da minuscolo a maiuscolo. Tuttavia, questo approccio non sarebbe altrettanto efficiente o semplice.

Per quanto riguarda i dettagli di implementazione, la funzione `to_uppercase` gestisce tutte le particolarità delle stringhe Unicode, che vanno oltre la semplice A-Z dell'alfabeto inglese. La standardizzazione Unicode è fondamentale per assicurarsi che la capitalizzazione funzioni correttamente in diversi alfabeti e lingue.

## See Also
- Articolo su Unicode e normalizzazione delle stringhe: [https://unicode.org/reports/tr15/](https://unicode.org/reports/tr15/)
- Guida alla conversione di stringhe in Rust, un linguaggio con cui Gleam condivide alcuni obiettivi di performance e sicurezza: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
