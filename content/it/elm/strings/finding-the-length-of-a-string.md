---
date: 2024-01-20 17:47:15.247992-07:00
description: 'How to: (Step by step) Elm rende semplice trovare la lunghezza di una
  stringa. Usa la funzione `String.length`.'
lastmod: '2024-04-05T21:53:44.112003-06:00'
model: gpt-4-1106-preview
summary: (Step by step) Elm rende semplice trovare la lunghezza di una stringa.
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to:
(Step by step)
Elm rende semplice trovare la lunghezza di una stringa. Usa la funzione `String.length`:

```Elm
module Main exposing (..)
import String

stringLength : String -> Int
stringLength str =
    String.length str

-- Uso
main =
    stringLength "Ciao, mondo!" 
    |> toString
    |> text
```

Output:

```
12
```

## Deep Dive:
(Curious insights)
La funzione `String.length` è diretta e affidabile, ma è interessante notare che contava i caratteri in modo diverso nelle prime versioni di Elm. Prima, calcolava la lunghezza in base a unità di codice UTF-16, cosa che poteva portare a conteggi errati con emoji o alcuni caratteri speciali. Ora, Elm misura correttamente i caratteri Unicode, rendendo il conteggio universale. Alcuni linguaggi offrono alternative, come loop personalizzati o funzioni basate su espressioni regolari, ma in Elm, `String.length` è la via da seguire.

## See Also:
(Fonti utili)
- Elm `String` package: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Discussione sulla misurazione della lunghezza delle stringhe Unicode: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
