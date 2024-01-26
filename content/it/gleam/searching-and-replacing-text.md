---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:57.487699-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Cercare e sostituire testo è una pratica comune per modificare informazioni in stringhe o file. I programmatori lo fanno per correggere errori, aggiornare dati o manipolare testi in modo efficiente.

## How to:
```Gleam
import gleam/string

// Cercare e sostituire in una stringa.
fn replace_in_string() {
  let original = "Ciao, mondo!"
  let updated = string.replace(original, "mondo", "universo")
  updated // "Ciao, universo!"
}

// Output del codice precedente.
pub fn main() {
  let result = replace_in_string()
  result |> io.println // Stampa "Ciao, universo!"
}
```

## Deep Dive
La ricerca e sostituzione testi esiste da quando i computer hanno cominciato a manipolare testi. In Gleam, la `string.replace` è una funzione diretta, parte del modulo `gleam/string`, per gestire queste operazioni. Alternative includono espressioni regolari e algoritmi di manipolazione tra cui edit distance e algoritmi di pattern matching come Boyer-Moore. L'implementazione Gleam è concepita per l'efficacia e sfrutta l'underlying runtime della BEAM machine (Erlang VM) per prestazioni affidabili.

## See Also
- Documentazione di Gleam: [https://gleam.run/](https://gleam.run/)
- Tutorial su espressioni regolari in Gleam: potrebbe non esserci un diretto riferimento a tutorial sulle regex in Gleam essendo che è un linguaggio relativamente nuovo, ma si può trovare supporto in forum e comunità di sviluppo.
