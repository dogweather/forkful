---
date: 2024-01-20 17:57:55.204566-07:00
description: "How to: Elm fornisce funzioni per lavorare con stringhe, ma non ha funzioni\
  \ native per la sostituzione di testo. Usiamo `String.Extra` dalla libreria\u2026"
lastmod: '2024-03-13T22:44:43.336173-06:00'
model: gpt-4-1106-preview
summary: Elm fornisce funzioni per lavorare con stringhe, ma non ha funzioni native
  per la sostituzione di testo.
title: Ricerca e sostituzione del testo
weight: 10
---

## How to:
Elm fornisce funzioni per lavorare con stringhe, ma non ha funzioni native per la sostituzione di testo. Usiamo `String.Extra` dalla libreria `elm-community/string-extra` per colmare questa lacuna.

```Elm
import String.Extra exposing (replace)

searchAndReplace : String -> String -> String -> String
searchAndReplace searchTerm replacement text =
    replace searchTerm replacement text

main =
    let
        originalText = "Ciao, Mondo!"
        newText = searchAndReplace "Mondo" "Elm" originalText
    in
    -- Stampa il nuovo testo
    newText
```

Output:

```
"Ciao, Elm!"
```

## Deep Dive
Elm, nato nel 2012 da Evan Czaplicki, enfatizza la sicurezza e la manutenibilità del codice. Non fornisce una funzione di sostituzione nativa perché predilige funzionalità core semplici e affidabili. Utilizzando `elm-community/string-extra`, otteniamo molte operazioni aggiuntive sulle stringhe, come `replace`.

Le alternative in JavaScript come il metodo `.replace()` o espressioni regolari non si adattano alla filosofia di Elm, concentrata sulla prevedibilità e sull'immunità dagli errori. In base allo stesso principio, `String.Extra.replace` non supporta espressioni regolari, ma effettua sostituzioni basate su stringhe pure.

Per implementare una funzione di ricerca e sostituzione nel tuo progetto Elm, ti affiderebbe alla composizione di funzioni, come mostrato nell'esempio `searchAndReplace`. Questo mantiene il tuo codice chiaro, testabile e mantenibile.

## See Also
- `elm-community/string-extra` su [Elm Package](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- Documentazione Elm su [String](https://package.elm-lang.org/packages/elm/core/latest/String)
