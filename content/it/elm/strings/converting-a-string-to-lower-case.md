---
date: 2024-01-20 17:38:13.667308-07:00
description: 'How to: Elm rende facile convertire le stringhe in minuscolo con la
  funzione `String.toLower`. Ecco come usarla.'
lastmod: '2024-03-13T22:44:43.337851-06:00'
model: gpt-4-1106-preview
summary: Elm rende facile convertire le stringhe in minuscolo con la funzione `String.toLower`.
title: Conversione di una stringa in minuscolo
weight: 4
---

## How to:
Elm rende facile convertire le stringhe in minuscolo con la funzione `String.toLower`. Ecco come usarla:

```Elm
import Html exposing (text)

main =
  let
    original = "Ciao Mondo!"
    lowercased = String.toLower original
  in
  text lowercased
```

Output:
```
ciao mondo!
```

## Deep Dive
La conversione di stringhe in minuscolo è un'operazione comune nei linguaggi di programmazione moderni. In Elm, questa funzionalità viene fornita dal modulo `String` che usa l'implementazione Unicode per gestire correttamente tutti i caratteri. È importante considerare che, in alcuni casi (come i caratteri Turchi), la conversione può non essere banale a causa di regole di localizzazione specifiche. Le alternative dirette come l'uso di codice ASCII per la conversione dei caratteri sono meno affidabili. Per esempio, Elm tratta le stringhe come sequenze di rune (code points Unicode), non come array di byte; quindi, l'approccio di conversione è più universale.

## See Also
- Documentazione Elm `String`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Unicode Case Folding: https://www.unicode.org/reports/tr44/#CaseFolding
- Stack Overflow Elm tag for community help: https://stackoverflow.com/questions/tagged/elm
