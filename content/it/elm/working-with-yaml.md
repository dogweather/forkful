---
title:                "Lavorare con YAML"
date:                  2024-01-19
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML è un formato di serializzazione dati leggibile dagli umani, usato per configurazioni, files di dumping e dati di messaggistica. Lo usiamo perché è semplice, leggibile e facilmente trasformabile in altre strutture di dati come JSON.

## How to:
Elm non ha una libreria core per analizzare YAML, dunque usiamo `elm-yaml` di `terezka`. Installa con:

```shell
elm install terezka/yaml
```

Codice di esempio per decodificare YAML:

```elm
import Yaml.Decode exposing (..)
import Dict exposing(Dict)

type alias Config =
    { name : String
    , items : Dict String Int
    }

configDecoder : Decoder Config
configDecoder =
    map2 Config
        (field "name" string)
        (field "items" (dict int))

configYaml : String
configYaml =
    """
    name: Example
    items:
        item1: 5
        item2: 10
    """

decodeResult : Result String Config
decodeResult =
    Yaml.Decode.fromString configDecoder configYaml
```

Output: `Ok { name = "Example", items = Dict.fromList [("item1", 5), ("item2", 10)] }`

## Approfondimento
YAML nasce nel 2001. È stato progettato per essere facile da leggere e supportare molteplici linguaggi di programmazione. JSON e XML sono alternative a YAML, ma sono meno leggibili. In Elm, la gestione di YAML è possibile ma richiede una libreria esterna perché non è inclusa nel nucleo del linguaggio.

## See Also
Approfondisci con queste risorse:

- `elm-yaml`: https://package.elm-lang.org/packages/terezka/yaml/latest/
- Specifiche YAML: https://yaml.org/spec/1.2/spec.html
- Confronto tra JSON e YAML: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON

Queste ti daranno una comprensione più approfondita di YAML e come usarlo con Elm.
