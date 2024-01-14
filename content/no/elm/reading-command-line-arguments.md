---
title:                "Elm: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har brukt et program fra kommandolinjen, har du sikkert sett at det er mulig å legge til en ekstra informasjon når du kjører programmet. Dette kalles kommandolinjeargumenter, og lar deg tilpasse programmet ditt basert på ulike inndata. Les videre for å lære mer om hvordan du kan lese disse argumentene i Elm.

## Hvordan

For å lese kommandolinjeargumenter i Elm, må du først importere "Platform" -modulen og deretter bruke funksjonen "programWithFlags". I eksempelet nedenfor, vil vi lese inn to tall fra kommandolinjen og addere dem sammen:

```Elm
import Platform

main =
  Platform.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model =
  { tall1 : Int
  , tall2 : Int
  }

init : (Model, Cmd Msg)
init =
  ( Model 0 0
  , Cmd.none
  )

type Msg = Addere

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Addere ->
      ( { model | tall1 = model.tall1 + model.tall2 }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ p [] [text ("Summen av tall1 og tall2 er: " ++ (toString (model.tall1 + model.tall2)))]
    , button [onClick Addere] [text "Adder"]
    ]

```

Når du kjører dette programmet fra kommandolinjen og legger til to tall som argumenter, vil du få ut en sum som tilsvarer tallene du skrev inn. For eksempel, hvis du bruker kommandoen "elm runtime adder 5 10", vil summen av tallene bli 5 + 10 = 15.

## Dypdykk

Det finnes flere måter å lese kommandolinjeargumenter på i Elm. Et alternativ er å bruke elf-formatet istedenfor JSON for flaggene, som gjør det mulig å ta i mot mer komplekse datastrukturer som argumenter.

For å lese mer om hvordan du kan bruke kommandolinjeargumenter i Elm, kan du se på dokumentasjonen til "Platform"-modulen og prøve ut ulike eksempler på egen hånd.

## Se også
- [https://package.elm-lang.org/packages/elm/core/latest/Platform#programWithFlags](https://package.elm-lang.org/packages/elm/core/latest/Platform#programWithFlags)
- [https://elmprogramming.com/command-line-arguments.html](https://elmprogramming.com/command-line-arguments.html)
- [https://github.com/elm/compiler/blob/master/hints/command-line-arguments.md](https://github.com/elm/compiler/blob/master/hints/command-line-arguments.md)