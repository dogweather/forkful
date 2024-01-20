---
title:                "Capitalizzare una stringa"
html_title:           "Elm: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Capitalizzare una stringa significa fare in modo che la prima lettera di ogni parola sia maiuscola. I programmatori lo fanno per migliorare la leggibilità e l'estetica del testo.

## Come Fare:

In Elm, potremmo non avere una funzione pronta per capitalizzare una stringa. Dovremmo scriverla da soli. Ecco un esempio utilizzando `Char.toUpper` e `String.uncons`.

```Elm
capitalize : String -> String
capitalize s =
    case String.uncons s of
        Nothing -> 
            ""
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest
```

Ecco come utilizzare questa funzione nel codice:

```Elm
module Main exposing (..)

import Html exposing (Html, text)
import String

capitalizedText : String -> Html msg
capitalizedText str =
    text (capitalize str)

main =
    capitalizedText "ciao mondo!" -- Risultato: "Ciao mondo!"
```

## Approfondimento

Avere ogni parola in una frase che inizia con una lettera maiuscola è una convenzione comune in molti linguaggi. Tuttavia, l'implementazione varia. Alcuni linguaggi, come JavaScript, hanno funzioni incorporate per farlo. Elm, d'altro canto, richiede una funzione personalizzata.

Un'alternativa per capitalizzare una stringa sarebbe convertire l'intera stringa in maiuscolo con `String.toUpper`, ma questo non è esattamente la stessa cosa. 

La funzione `capitalize` che abbiamo scritto sopra prende il primo carattere della stringa, lo trasforma in maiuscolo, e poi lo attacca al resto della stringa. 

## Per Ulteriori Informazioni

- Elm: [Documentazione ufficiale](https://elm-lang.org/docs)
- `Char.toUpper`: [Elm - Funzione Char.toUpper](https://package.elm-lang.org/packages/elm/core/latest/Char)