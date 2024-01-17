---
title:                "Läsa kommandoradsargument"
html_title:           "Elm: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av kommandoradsargument är en vanlig process inom programmering där programmet tar in parametrar som anges av användaren via terminalen. Detta är ett effektivt sätt för programmerare att anpassa och styra programmet utifrån användarens önskemål.

## Så här:
```Elm
import Html exposing (text)
import Html.Attributes exposing (..)

import Task exposing (..)
import Task.Cmd exposing (run)
import Platform.Cmd exposing (Cmd, none)
import Platform.Cmd.Internals exposing (Cmd, Task)
import Cmd

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
  
init : (Model, Cmd Msg)
init =
  (Model initialConfig, Cmd.none)
  
type alias Model =
  { config : Config }
  
type alias Config =
  { options : List String }
  
type Msg
  = SetOptions (Model -> (Model, Cmd Msg))
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetOptions f ->
      f model
      |> Task.perform (always Nothing)
  
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  
view : Model -> Html Msg
view model =
  div [ style "font-family" "Arial, sans-serif"
      , style "font-size" "14px"
      ]
    [ input [ type_ "text"
            , onInput SetOptions
            ] []
    , div []
      [ text "Ange önskade options: "
      , span [] [ text (String.join " " model.config.options) ]
      ]
    ]

```

Exempel på hur användaren kan ange kommandoradsargument och hur det påverkar programmet:
```
$ elm make Main.elm
    `options` set to an empty list
$ elm make Main.elm --optimize
    `options` set to ["optimize"]
$ elm make Main.elm --debug --port=8000
    `options` set to ["debug", "port=8000"]
```

## Djupdykning:
Historiskt sett har läsning av kommandoradsargument varit en vanlig funktion inom programmering, men med ökande popularitet av grafiska användargränssnitt (GUI) har dess relevans minskat. Alternativ till att läsa kommandoradsargument inkluderar att använda menyval eller knappar i ett GUI, men för vissa program är läsning av kommandoradsargument fortfarande det mest lämpliga valet.

Implementationen av läsning av kommandoradsargument kan variera beroende på programmeringsspråk, men i grund och botten innebär det att läsa in parametrar från terminalen och översätta dem till en struktur som programmet kan förstå och använda.

## Se även:
- [Officiell Elm dokumentation om Cmd](https://package.elm-lang.org/packages/elm/core/latest/Cmd)
- [GeeksforGeeks artikel om läsning av kommandoradsargument i Elm](https://www.geeksforgeeks.org/reading-command-line-arguments-in-elm/)