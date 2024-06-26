---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:54.287494-07:00
description: "Come fare: In Elm, non esiste una funzione incorporata specificamente\
  \ per capitalizzare le stringhe. Tuttavia, puoi ottenere facilmente questo risultato\u2026"
lastmod: '2024-03-13T22:44:43.334297-06:00'
model: gpt-4-0125-preview
summary: In Elm, non esiste una funzione incorporata specificamente per capitalizzare
  le stringhe.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
In Elm, non esiste una funzione incorporata specificamente per capitalizzare le stringhe. Tuttavia, puoi ottenere facilmente questo risultato utilizzando le funzioni del modulo `String` incorporate come `toUpper`, `toLower`, `left` e `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Esempio di utilizzo
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Output: "Hello World"
```

Per scenari più complessi o se preferisci utilizzare una libreria che fornisce un modo diretto per capitalizzare le stringhe, potresti considerare un pacchetto di terze parti come `elm-community/string-extra`. Tuttavia, come da ultimo aggiornamento, l'ecosistema di Elm incoraggia a gestire tali compiti usando funzioni incorporate per mantenere il linguaggio e i progetti snelli.

```elm
import String.Extra as StringExtra

-- Nel caso ci sia una funzione `capitalize` in una libreria di terze parti
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Esempio di utilizzo con funzione ipotetica di libreria
main =
    "this is elm" |> capitalizeWithLibrary
    -- Output ipotetico: "This is elm"
```

Controlla sempre il repository dei pacchetti di Elm per le librerie più recenti e preferite per la manipolazione delle stringhe se stai cercando funzionalità aggiuntive rispetto alla libreria standard.
