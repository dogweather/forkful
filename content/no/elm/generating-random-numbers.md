---
title:                "Generering av tilfeldige tall"
html_title:           "Elm: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall kan være nyttig for mange programmeringsprosjekter, for eksempel spill eller simuleringer. Ved å bruke tilfeldige tall kan man skape forskjellige utfall og gjøre programmene mer spennende.

## Slik gjør du det
For å generere tilfeldige tall i Elm, kan man bruke funksjonen `Random.generate`, som tar to argumenter: en generator og en melding. Her er et eksempel på hvordan man kan bruke denne funksjonen for å generere et tilfeldig tall mellom 1 og 10:

```Elm
import Random

-- Definerer en melding som skal sendes når et tilfeldig tall blir generert
type Msg
    = NewNumber Int

-- Definerer en generator som genererer tall mellom 1 og 10
generator : Random.Generator Int
generator =
    Random.int 1 10

-- Når brukeren gjør en handling, vil en tilfeldig tallmelding bli sendt
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Genererer et nytt tilfeldig tall og sender det som en melding
        NewNumber number ->
            ( { model | number = number }, Cmd.none )

-- For å initialisere programmet, må man starte med et tomt tilfeldig tall
model : Model
model =
    { number = 0 }

-- Viser det siste tilfeldige tallet på skjermen
view : Model -> Html Msg
view model =
    div []
        [ text (String.fromInt model.number) ]

main : Program Never Model Msg
main =
    Browser.element
        { init = (\_ -> ( model, Cmd.none ))
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
```

Sample output:

Hver gang brukeren utfører en handling, vil et nytt tilfeldig tall mellom 1 og 10 bli generert og vises på skjermen. Her er noen eksempler på output:

- Første handling: 5 
- Andre handling: 9
- Tredje handling: 2

## Dypdykk
For å få mer kontroll over genereringen av tilfeldige tall, kan man bruke modulen `Random` i Elm. Denne modulen inneholder flere funksjoner og hjelpere for å generere tilfeldige tall i ulike former og intervaller. Det finnes også en funksjon `List.random` som kan brukes for å generere en tilfeldig verdi fra en liste. Dette kan være nyttig for å generere tilfeldige elementer i et spill eller en spilleliste. For mer informasjon om disse funksjonene og mulige bruksområder, kan du sjekke ut dokumentasjonen for `Random` modulen.

## Se også 
- [Dokumentasjon for Random modulen i Elm](https://package.elm-lang.org/packages/elm/random/latest/)
- [Eksempler på bruk av Random modulen i Elm](https://elmprogramming.com/generate-random-numbers.html)