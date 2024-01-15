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

## Perché

Vuoi semplificare la gestione dei dati strutturati nel tuo progetto Elm? Allora devi imparare a lavorare con YAML!

## Come

Usando la libreria `elm-tools/parser` puoi facilmente parsare un documento YAML in un valore Elm.

```Elm
import Parser exposing (..)
import YAML exposing (..)

yamParser : Parser (YAM Document)
yamParser =
  yamlDocument
    |> fromString "title: Hello World!\n
                  body: This is an article written in Elm"

result : Result (List (Error YAM.Error)) (YAM Document)
result =
  parse yamParser

-- Result = Ok [ { title = "Hello World!", body = "This is an article written in Elm" } ]

```

## Deep Dive

Una volta che hai parsato un documento YAML, puoi facilmente accedere ai singoli valori utilizzando la loro chiave come indice. Puoi anche creare funzioni personalizzate per manipolare i dati secondo le tue esigenze.

```Elm
import Parser
import YAML exposing (..)

type alias Person = 
  { name : String 
  , age : Int
  , profession : String
  }

personParser : Parser Person
personParser =
  yamlDocument
    |> andThen (\yaml -> 
      Parser.succeed Person
        |. field "name" string
        |. field "age" int
        |. field "profession" string
    )

result : Result (List (Error YAM.Error)) Person
result =
  parse personParser
```

## Vedi anche

- Documentazione ufficiale della libreria `elm-tools/parser`: https://package.elm-lang.org/packages/elm-tools/parser/latest/
- Documentazione ufficiale della libreria YAML per Elm: https://package.elm-lang.org/packages/mdgriffith/elm-node-widgets/latest/