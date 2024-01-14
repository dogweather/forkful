---
title:                "Elm: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor 

I dagens verden av programmering er det viktig å kunne tilfeldige tall. Kanskje du vil simulere et spill, lage et lotteri, eller generere unike koder. Uansett hva årsaken er, kan tilfeldige tall være en viktig del av mange programmeringsprosjekter. I denne bloggposten vil vi utforske hvordan du kan generere tilfeldige tall ved hjelp av Elm-programmeringsspråket.

# Hvordan

For å generere tilfeldige tall i Elm, må vi bruke en "Random"-modul. Dette gir oss funksjoner for å lage tilfeldige tall basert på forskjellige typer distribusjoner. La oss se på et eksempel på hvordan du kan generere et tilfeldig tall fra 1 til 10:

```Elm
import Random

randomNum : Int
randomNum =
    Random.int 1 10
```

Her importerer vi modulen og bruker funksjonen "int" for å generere et heltall mellom 1 og 10. Vi kan også bruke "float" funksjonen for å generere et tilfeldig flyttall. For å faktisk få et tilfeldig tall, må vi bruke funksjonen "generate" som tar inn funksjonen vår og returnerer et tilfeldig tall.

```Elm
import Random

randomNum : Cmd msg
randomNum =
    Random.generate NewNumber (Random.int 1 10)

type Msg
    = NewNumber Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewNumber num ->
            ( { model | randomNum = num }, Cmd.none )
```

Her har vi opprettet en handling "NewNumber" som tar inn et heltall og oppdaterer modellen vår. Vi bruker også funksjonen "Cmd.none" for å unngå å sende en kommando til elm-runtime som kan forårsake en uendelig løkke.

# Dypdykk

"Random"-modulen i Elm har muligheter for å generere tall fra forskjellige distribusjoner som "normal", "uniform" og "exponential". Vi kan også bruke funksjonen "step" for å generere sekvensielle tilfeldige tall basert på en gitt distribusjon. Utforsk dokumentasjonen for mer informasjon og eksempler.

# Se også

- Elm Dokumentasjon: https://guide.elm-lang.org/effects/random.html
- Random-mehodene: https://package.elm-lang.org/packages/elm/random/latest/Random
- Youtube tutorial: https://www.youtube.com/watch?v=is895OMo5FQ