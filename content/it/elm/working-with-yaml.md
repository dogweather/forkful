---
title:                "Lavorare con yaml"
html_title:           "Elm: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il YAML è un formato di file leggibile dall'uomo e dalla macchina che è comunemente utilizzato per la configurazione dei programmi e lo scambio di dati strutturati. I programmatori spesso scelgono di utilizzare il YAML perché è più facile da leggere rispetto ad altri formati come JSON e XML, ed è più flessibile e modulare rispetto alla configurazione diretta nel codice.

## Come fare:

```Elm
import Json.Decode exposing (decodeValue)
import Yaml exposing (load)

type alias Config =
    { firstName : String
    , lastName : String
    , age : Int
    }

configDecoder : Decode.Decoder Config
configDecoder =
    Decode.map3 Config
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "age" Decode.int)

env = "firstName: John\nlastName: Doe\nage: 30"

config : Result String Config
config =
    env
        |> load
        |> decodeValue configDecoder
```

Il codice sopra mostra come caricare un file YAML e decodificarlo in un tipo di dati Elm. Viene utilizzata la libreria Yaml per caricare il file e la funzione `Decode.map3` per effettuare il parsing dei dati nel tipo di dati `Config`. Infine, viene utilizzato l'operatore `Result` per gestire i possibili errori di decodifica.

## Approfondimenti:

Il formato YAML è stato introdotto nel 2001 ed è stato creato con l'obiettivo di essere più leggibile per gli esseri umani rispetto ad altri formati come JSON e XML. Alcune alternative al YAML includono TOML e INI, ma entrambi sono meno flessibili e modulari. 

Il modulo Elm Yaml utilizza la libreria di parser JS-YAML per effettuare il parsing dei file YAML. Inoltre, è in grado di convertire i dati decodificati in tipi di dati Elm, rendendo più semplice l'utilizzo dei dati all'interno del tuo programma.

## Vedi anche:

Per ulteriori informazioni sul formato YAML, è possibile consultare il sito ufficiale all'indirizzo https://yaml.org/. Per informazioni specifiche sulla libreria Yaml di Elm, è possibile visitare la sua pagina su Elm Package Docs: https://package.elm-lang.org/packages/yiito/elm-yaml/latest/