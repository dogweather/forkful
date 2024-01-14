---
title:    "Elm: Å starte et nytt prosjekt"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt programmeringsprosjekt kan være en spennende og utfordrende oppgave. Med Elm, et funksjonelt programmeringsspråk utviklet for å lage webapplikasjoner, kan du få en annerledes og kraftig tilnærming til å lage nettsider og applikasjoner. I denne bloggposten vil vi gi en oversikt over hva Elm er og hvordan du kan komme i gang med å lage dine egne prosjekter.

## Slik gjør du det

For å komme i gang med Elm, trenger du først å installere det på datamaskinen din. Dette kan gjøres ved å følge instruksjonene på Elm sine offisielle nettsider. Når Elm er installert, kan du åpne et nytt prosjekt ved å skrive følgende i terminalen din:

```Elm init```

Dette vil opprette en ny mappe med all kode du trenger for å begynne å kode i Elm. Deretter kan du åpne denne mappen i en teksteditor og begynne å skrive din første kode.

Et eksempel på en enkel Elm-applikasjon kan være å lage en side med en knapp som, når den blir trykket på, endrer en tekst på siden. Her er en enkel kode som gjør akkurat det:

```Elm
import Browser exposing (element, Html)
import Html exposing (..)
import Html.Events exposing (onClick)

type Msg = ButtonClicked

update: Msg -> Model -> Model
update msg model =
  case msg of
    ButtonClicked ->
      { model | buttonText = "Ny tekst på siden!" }

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick ButtonClicked ] [ text "Trykk meg!" ]
    , div [] [ text model.buttonText ]
    ]

main: Program () Model Msg
main =
  Browser.element
    { init = { buttonText = "Hei fra Elm!" }
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
```

Koden over vil gi en nettside med en knapp som endrer teksten på siden når den blir trykket på. Dette er en enkel måte å vise Elm sin funksjonelle tilnærming til webutvikling.

## Dypdykk

For å lære enda mer om Elm og hvordan du kan få mest mulig ut av det, anbefaler vi å utforske dokumentasjonen på deres offisielle nettsider. Her finner du alt du trenger å vite om språket og hvordan du kan bruke det til å lage fantastiske webapplikasjoner.

Her er noen andre ressurser som kan være nyttige å sjekke ut når du starter med Elm:

- Elm slack community: https://elmlang.slack.com/
- Elm forum: https://discourse.elm-lang.org/
- Elm skoleringsressurser: https://elm-lang.org/resources
- Offisiell Elm dokumentasjon: https://guide.elm-lang.org/

Lykke til med å lage spennende prosjekter med Elm!

## Se også

- Offisiell Elm nettside: https://elm-lang.org/
- Elm eksempelprosjekter: https://github.com/elm-lang/examples/
- Elm pakker og moduler: https://package.elm-lang.org/
- Elm community ressurser: https://elm-community.github.io/