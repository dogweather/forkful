---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, JavaScript Object Notation, è un formato dati leggero per lo scambio di dati. I programmatori lo usano per comunicare con server web e per salvare informazioni in modo strutturato.

## How to:
Elm rende la gestione di JSON semplice grazie al suo sistema di tipi. Ecco un esempio che decodifica un oggetto JSON:

```Elm
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

jsonString : String
jsonString =
    """{"id": 1, "name": "Alice"}"""

decodedUser : Result String User
decodedUser =
    Decode.decodeString userDecoder jsonString
```
Se `jsonString` è formattato correttamente, `decodedUser` sarà `Ok { id = 1, name = "Alice" }`. Altrimenti, otterremo un errore di decodifica.

## Deep Dive
Elm ha adottato JSON come mezzo principale per lo scambio di dati da quando è stato creato. Mentre in JavaScript l'analisi di JSON è nativa, in Elm usiamo decoder per trasformare JSON in tipi Elm sicuri. JSON è la scelta per default, ma si può usare anche XML o un altro formato, anche se richiederebbe più lavoro. Elm forza una chiara corrispondenza tra i dati JSON e la struttura dei dati Elm, facendo debugging e manutenzione più facili.

## See Also
- Documentazione ufficiale di Elm su JSON: [Elm - JSON](https://package.elm-lang.org/packages/elm/json/latest/)
- Comunità Elm su Reddit, per discussioni e domande: [Reddit - Elm](https://www.reddit.com/r/elm/)
