---
title:                "Capitalizzare una stringa"
aliases:
- /it/elm/capitalizing-a-string.md
date:                  2024-02-03T19:04:54.287494-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

La capitalizzazione di una stringa comporta la trasformazione del carattere iniziale di una stringa data in maiuscolo, mantenendo il resto in minuscolo, spesso a fini di formattazione standardizzata o leggibilità. I programmatori eseguono frequentemente questo compito per garantire che i dati siano presentati in modo consistente, specialmente nelle interfacce utente o quando elaborano e visualizzano l'input dell'utente.

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
