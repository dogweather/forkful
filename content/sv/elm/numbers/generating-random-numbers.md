---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:35.453782-07:00
description: "Att generera slumpm\xE4ssiga nummer i Elm inneb\xE4r att anv\xE4nda\
  \ `Random`-modulen f\xF6r att producera pseudo-slumpm\xE4ssiga nummer, vilket \xE4\
  r anv\xE4ndbart f\xF6r en\u2026"
lastmod: '2024-03-13T22:44:37.825133-06:00'
model: gpt-4-0125-preview
summary: "Att generera slumpm\xE4ssiga nummer i Elm inneb\xE4r att anv\xE4nda `Random`-modulen\
  \ f\xF6r att producera pseudo-slumpm\xE4ssiga nummer, vilket \xE4r anv\xE4ndbart\
  \ f\xF6r en m\xE4ngd uppgifter s\xE5som spel, simuleringar och \xE4ven som en del\
  \ av algoritmer som kr\xE4ver stokastiska processer."
title: "Generera slumpm\xE4ssiga tal"
weight: 12
---

## Vad & Varför?
Att generera slumpmässiga nummer i Elm innebär att använda `Random`-modulen för att producera pseudo-slumpmässiga nummer, vilket är användbart för en mängd uppgifter såsom spel, simuleringar och även som en del av algoritmer som kräver stokastiska processer. Denna förmåga tillåter utvecklare att lägga till oförutsägbarhet och variation i sina applikationer, vilket förbättrar användarupplevelse och funktionalitet.

## Hur man gör:
Elms rena funktionella natur innebär att du inte kan generera slumpmässiga nummer direkt som du kanske kan i imperativa språk. Istället använder du `Random`-modulen tillsammans med kommandon. Här är ett grundläggande exempel som genererar ett slumpmässigt heltal mellan 1 och 100.

Först, installera `Random`-modulen med `elm install elm/random`. Importera sedan den till din Elm-fil, tillsammans med de nödvändiga HTML- och händelsemodulerna, så här:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

För att detta ska vara ett självständigt exempel kan du lägga till denna mallkod:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Nästa steg, definiera ett **kommando** för att generera ett slumpmässigt nummer. Detta innebär att ställa in en `Msg`-typ för att hantera det slumpmässiga numret när det har genererats, en `Model` för att lagra det, och en uppdateringsfunktion för att sammanbinda allt.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

För att utlösa en nummergenerering skulle du skicka ett `Generate`-meddelande, till exempel genom en knapp i din vy:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Slumpmässigt Nummer: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generera" ]
        ]
```

När du klickar på "Generera"-knappen, kommer ett slumpmässigt nummer mellan 1 och 100 att visas.

Detta förenklade tillvägagångssätt kan anpassas och utvidgas, genom att utnyttja andra funktioner i `Random`-modulen för att producera slumpmässiga flyttal, listor eller till och med komplexa datastrukturer baserat på anpassade typer, vilket ger en stor lekplats för att lägga till oförutsägbarhet i dina Elm-applikationer.

Elm-guiden går in på mycket mer detaljer. Den har också [ett exempel på att rulla en sexsidig tärning](https://guide.elm-lang.org/effects/random).
