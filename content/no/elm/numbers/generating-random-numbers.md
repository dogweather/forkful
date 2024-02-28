---
title:                "Generere tilfeldige tall"
date:                  2024-02-27T22:50:34.412858-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å generere tilfeldige tall i Elm innebærer å bruke `Random`-modulen for å produsere pseudo-tilfeldige tall, som er hendige for en rekke oppgaver som spill, simuleringer og selv som en del av algoritmer som krever stokastiske prosesser. Denne muligheten lar utviklere legge til uforutsigbarhet og variasjon i applikasjonene deres, noe som forbedrer brukeropplevelse og funksjonalitet.

## Hvordan:
Elms rene funksjonelle natur betyr at du ikke kan generere tilfeldige tall direkte som du kanskje ville gjort i imperativ språk. I stedet bruker du `Random`-modulen i samspill med kommandoer. Her er et grunnleggende eksempel som genererer et tilfeldig heltall mellom 1 og 100.

Først, installer `Random`-modulen med `elm install elm/random`. Deretter importerer du den inn i din Elm-fil, sammen med de nødvendige HTML- og hendelsesmodulene, slik:

`src/Main.elm`

```elm
modul Main eksponerer (..)

importerer Browser
importerer Html eksponerer (Html, button, text, div)
importerer Html.Events eksponerer (onClick)
importerer Random
```

For at dette skal være et selvstendig eksempel, kan du legge til denne malen:
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

Neste, definer et **kommando** for å generere et tilfeldig tall. Dette innebærer å sette opp en `Msg`-type for å håndtere det tilfeldige tallet når det er generert, en `Model` for å lagre det, og en oppdateringsfunksjon for å binde alt sammen.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    tilfellet msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom nummer ->
            ( { model | randomNumber = nummer }, Cmd.none )
```

For å utløse generering av et tall, ville du sendt en `Generate`-melding, for eksempel gjennom en knapp i visningen din:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Tilfeldig tall: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generer" ]
        ]
```

Når du klikker på "Generer"-knappen, vil et tilfeldig tall mellom 1 og 100 vises.

Dette forenklede tilnærmingen kan tilpasses og utvides, som utnytter andre funksjoner i `Random`-modulen for å produsere tilfeldige flyttall, lister eller til og med komplekse datatrukturer basert på egendefinerte typer, og gir et stort lekeområde for å legge til uforutsigbarhet i Elm-applikasjoner.

Elm-guiden går inn i mye mer detalj. Den har også [et eksempel på å kaste en sekssidig terning](https://guide.elm-lang.org/effects/random).
