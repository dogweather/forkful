---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:13.618240-07:00
description: "Het genereren van willekeurige getallen in Elm houdt het gebruik van\
  \ de `Random` module in om pseudo-willekeurige getallen te produceren, die handig\
  \ zijn\u2026"
lastmod: '2024-03-13T22:44:50.720443-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in Elm houdt het gebruik van de\
  \ `Random` module in om pseudo-willekeurige getallen te produceren, die handig zijn\u2026"
title: Genereren van willekeurige getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Het genereren van willekeurige getallen in Elm houdt het gebruik van de `Random` module in om pseudo-willekeurige getallen te produceren, die handig zijn voor een verscheidenheid aan taken zoals spellen, simulaties, en zelfs als onderdeel van algoritmen die stochastische processen vereisen. Deze mogelijkheid stelt ontwikkelaars in staat onvoorspelbaarheid en variëteit aan hun applicaties toe te voegen, wat de gebruikerservaring en functionaliteit verbetert.

## Hoe:
De pure functionele aard van Elm betekent dat je niet direct willekeurige getallen kunt genereren zoals je misschien zou doen in imperatieve talen. In plaats daarvan gebruik je de `Random` module in combinatie met commando's. Hier is een basisvoorbeeld dat een willekeurig geheel getal tussen 1 en 100 genereert.

Installeer eerst de `Random` module met `elm install elm/random`. Importeer het vervolgens in je Elm-bestand, samen met de benodigde HTML- en event-modules, zoals zo:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Om dit een zelfstandig voorbeeld te maken, kun je dit sjabloon toevoegen:
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

Definieer vervolgens een **commando** om een willekeurig getal te genereren. Dit houdt in dat je een `Msg` type instelt om het willekeurige getal te behandelen zodra het is gegenereerd, een `Model` om het op te slaan, en een update-functie om alles samen te binden.
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

Om een getallengeneratie te activeren, zou je een `Generate` bericht sturen, bijvoorbeeld via een knop in je weergave:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Willekeurig Getal: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Genereer" ]
        ]
```

Wanneer je op de "Genereer" knop klikt, wordt een willekeurig getal tussen 1 en 100 weergegeven.

Deze simplistische benadering kan worden aangepast en uitgebreid, door gebruik te maken van andere functies in de `Random` module om willekeurige drijvende komma getallen, lijsten of zelfs complexe datastructuren te produceren op basis van aangepaste typen, waardoor een uitgebreide speeltuin wordt geboden voor het toevoegen van onvoorspelbaarheid aan je Elm-applicaties.

De Elm-gids gaat veel meer in detail. Er is ook [een voorbeeld van het rollen van een zeszijdige dobbelsteen](https://guide.elm-lang.org/effects/random).
