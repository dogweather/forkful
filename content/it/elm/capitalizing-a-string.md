---
title:                "Maiuscolizzare una stringa"
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Capitalizzare una stringa significa trasformare tutte le lettere in maiuscolo. Questo serve a uniformare i dati per confronti o visualizzazioni e a rimarcare l'importanza di certi testi.

## How to: (Come fare:)
Per capitalizzare una stringa in Elm, utilizza la funzione `String.toUpper`. Ecco un esempio:

```Elm
import String

capitalizedString : String -> String
capitalizedString string =
    String.toUpper string

-- Uso della funzione
main =
    Html.text (capitalizedString "ciao mondo!")

-- Risultato stampato: "CIAO MONDO!"
```

## Deep Dive (Approfondimento)
Capitlizzare stringhe è un'operazione antica quanto la programmazione. In Elm, la funzionalità è diretta e senza fronzoli grazie alla standard library. Alternative? Beh, potresti iterare carattere per carattere e capitalizzarli individualmente, ma non c'è motivo di complicarsi la vita: `String.toUpper` fa il lavoro per te. Internamente, gestisce anche correttamente le lettere accentate e i caratteri speciali.

## See Also (Vedi Anche)
- Documentazione ufficiale Elm su `String.toUpper`: [String.toUpper](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)