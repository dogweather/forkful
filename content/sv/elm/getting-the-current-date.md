---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få nuvarande datum är processen för att hämta dagens datum från systemklockan. Programmerare gör detta för att logga händelser, generera tidsstämplar, eller för att utföra tidsbaserade beräkningar.

## Hur man gör:
I Elm 0.19, skapar vi en kommando (`Cmd`) använd för att begära dagens datum från JavaScript genom Elm’s `Time` modul. 

```Elm
port module Main exposing (..)

import Browser
import Html exposing (Html)
import Task
import Time exposing (Posix)
import Process

port currentTime : () -> Cmd msg

type alias Model = {
    time : Maybe Posix
}

initialModel : Model
initialModel = { time = Nothing }

type Msg
    = ReceiveTime Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveTime newTime ->
            ({ model | time = Just newTime }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    currentTimeSub ReceiveTime

port currentTimeSub : (Posix -> msg) -> Sub msg

main =
    Browser.element 
        { init = \_ -> 
            ( initialModel, currentTime () )
        , view = view 
        , update = update
        , subscriptions = subscriptions
        }
```

När Elm app startar, skickar den ett `currentTime` kommando till JavaScript för att få nuvarande tidstämpel. Sedan används en prenumeration för att lyssna på `currentTimeSub` kanalen och uppdatera Elm modellen med nya tidsstämplar.

Du kan sedan använda `Date.fromTime` funktion för att konvertera `Posix` tid till en `Date`. 

## Djupdykning
Att hämta det aktuella datumet i Elm var enklare i tidigare versioner av språket. I Elm 0.18, fanns det en `Time.now` funktion som returnerade nuvarande tid. Men `Time.now` avlägsnades i Elm 0.19 till förmån för en bättre hantering av den globala tillståndet och en förbättrad testbarhet.

Alternativt, om din användning är icke-kritisk, kaan du välja att hämta tiden från systemet när Elm app startar. Se till att du är medveten om att denna tid kommer att vara konstant under appens livstid.

## Se Också
Följande resurser kan hjälpa dig att förstå mer om hur Elm hanterar tid:

1. Elm’s officiella dokumentation om `Time` modulen: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)

2. Elm's `Process` module: [https://package.elm-lang.org/packages/elm/core/latest/Process](https://package.elm-lang.org/packages/elm/core/latest/Process)