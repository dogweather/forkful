---
date: 2024-01-20 17:34:23.491440-07:00
description: "How to: (Come fare:) In Elm, concatenare le stringhe \xE8 semplice come\
  \ usare l'operatore `++`. Nato da linguaggi funzionali come Haskell, Elm promuove\
  \ un\u2026"
lastmod: '2024-04-05T22:50:57.167648-06:00'
model: gpt-4-1106-preview
summary: "(Come fare:) In Elm, concatenare le stringhe \xE8 semplice come usare l'operatore\
  \ `++`."
title: Concatenazione di stringhe
weight: 3
---

## How to: (Come fare:)
```Elm
import Html exposing (text)

main =
    let
        saluto = "Ciao"
        destinazione = "mondo!"
        fraseCompleta = saluto ++ " " ++ destinazione
    in
    text fraseCompleta
```

Output:
```
Ciao mondo!
```

## Deep Dive (Approfondimento)
In Elm, concatenare le stringhe è semplice come usare l'operatore `++`. Nato da linguaggi funzionali come Haskell, Elm promuove un approccio immutabile: le stringhe originali restano invariate. Altre alternative includono il modulo `String` con funzioni come `String.concat` o l'uso di template strings in altri linguaggi. Dal punto di vista dell'implementazione, concatenare stringhe può variare per efficienza, specialmente con stringhe grandi, perché il processo potrebbe richiedere la creazione di nuove stringhe e quindi più memoria ed elaborazione.

## See Also (Vedi Anche)
- Documentazione Elm su Stringhe: [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String)
- Una discussione su Elm Discourse riguardo l'efficienza della concatenazione delle stringhe: [Elm Discourse Thread](https://discourse.elm-lang.org/)
- Tutorial Elm: [Elm Tutorial](https://guide.elm-lang.org/)
