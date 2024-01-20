---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari sono modelli usati per corrispondere a stringhe di testo. I programmatori le utilizzano per validare, estrarre o sostituire testo, rendendo le operazioni su stringhe efficienti e potenti.

## How to:
Per utilizzare le espressioni regolari in Gleam, dobbiamo usare il pacchetto `gleam_regex`. Esempio di ricerca di numeri in una stringa:

```gleam
import gleam/regex

pub fn main() {
  let pattern = regex.from_string("\\d+") // Compila il pattern per numeri
  case pattern {
    Ok(regex) ->
      regex.find_all("Hai 3 messaggi e 4 notifiche.")
    Error(_) ->
      []
  }
}
```

Output di esempio:

```
["3", "4"]
```

Per sostituire testo usando una regex:

```gleam
import gleam/regex.{replace}

pub fn main() {
  let my_text = "Hello, world!"
  let my_regex = regex.from_string("world")? // '?' gestisce l'errore
  replace(my_regex, my_text, "Gleam")
}
```

Output di esempio:

```
"Hello, Gleam!"
```

## Deep Dive
Le espressioni regolari hanno origine negli anni '50 e sono diventate uno strumento standard in programmazione. Alternativamente, puoi usare funzioni di stringa native o parser dedicati, ma spesso sono meno flessibili. Gleam utilizza il pacchetto `regex` che compila le regex in codice che manipola le stringhe efficacemente.

## See Also
- Tutorial interattivi per le regex in generale: [https://regexone.com/](https://regexone.com/)