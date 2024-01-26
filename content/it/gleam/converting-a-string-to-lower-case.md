---
title:                "Conversione di una stringa in minuscolo"
date:                  2024-01-20T17:38:26.742450-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa semplicemente trasformare tutti i caratteri di una stringa in lettere minuscole. I programmatori fanno ciò per uniformare i dati, specialmente quando confrontano stringhe dove la capitalizzazione potrebbe variare.

## How to:
Gleam non ha una standard library propria per il trattamento delle stringhe, quindi, dobbiamo affidarci alle funzioni di Erlang. Ecco un esempio di come trasformare una stringa in minuscolo:

```gleam
import gleam/erlang

pub fn to_lower_case(string: String) -> String {
  erlang.string:to_lower(string)
}

pub fn main() {
  let my_string = "CiAO Gleam!"
  to_lower_case(my_string) |> io.debug
}
```

Output:

```
"ciao gleam!"
```

## Deep Dive
Storicamente, la conversione in minuscolo è stata una necessità per garantire che il confronto tra stringhe fosse case-insensitive, ovvero indipendente dalle maiuscole. 

Le alternative includono l'uso di espressioni regolari o la creazione di mappature personalizzate dei caratteri, ma ciò può essere più oneroso da implementare.

In termini di dettagli dell'implementazione, Gleam permette di attingere alle funzioni degli string di Erlang, che lavorano in maniera efficiente con la codifica UTF-8, assicurando che anche caratteri speciali e accentati vengano convertiti correttamente.

## See Also
Per ulteriori informazioni e per approfondire il trattamento delle stringhe in Erlang e Gleam, ecco alcune risorse utili:

- Documentazione di Erlang sulle stringhe:
  [Erlang -- String](http://erlang.org/doc/man/string.html)
  
- Documentazione ufficiale di Gleam:
  [Gleam Documentation](https://gleam.run/)

- Una discussione sulla mailing list di Erlang riguardante la manipolazione delle stringhe:
  [Erlang Questions -- String to Lowercase](http://erlang.org/pipermail/erlang-questions/)
