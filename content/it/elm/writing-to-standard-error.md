---
title:                "Scrivere su standard error"
html_title:           "Elm: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può sembrare un'azione banale ma è uno strumento utile per gestire errori e debug in Elm. È un'abilità importante da avere per migliorare la qualità del tuo codice e risolvere eventuali problemi che possono sorgere durante lo sviluppo.

## Come Fare

Per scrivere su standard error in Elm, puoi utilizzare la funzione `Debug.log`. Questa funzione accetta due argomenti: una stringa che rappresenta un messaggio di debug e un qualsiasi valore che si desidera visualizzare. Ad esempio:

```Elm
import Debug exposing (log)

favoriteLanguage = "Elm"

log "Il mio linguaggio preferito è" favoriteLanguage
```

L'output sarebbe: `Il mio linguaggio preferito è Elm`. Questo può aiutare a monitorare lo stato delle variabili all'interno del tuo codice e a identificare eventuali problemi.

## Approfondimento

È importante notare che `Debug.log` viene compilato solo quando si utilizzano le opzioni di debugging e non verrà incluso nella versione finale del tuo codice. Inoltre, questa funzione utilizza un buffer, quindi gli output possono non essere immediatamente visibili. Per visualizzare immediatamente gli output su standard error, puoi utilizzare la funzione `Platform.debug` aggiungendo `<|`, ad esempio:

```Elm
import Platform

favoriteLanguage = "Elm"

Platform.debug <| "Il mio linguaggio preferito è" ++ favoriteLanguage
```

Puoi anche passare più di un valore alla funzione, basta utilizzare una virgola per separarli.

## Vedi Anche

- [Documentazione di `Debug` (Inglese)](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Risorse su debugging in Elm (Inglese)](https://elmprogramming.com/debugging-elm-applications.html)