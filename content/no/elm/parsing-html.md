---
title:                "Analysering av html"
html_title:           "Elm: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Å parse HTML kan være svært nyttig når man jobber med webutvikling. Det lar oss hente ut og manipulere data fra nettsider, noe som kan være essensielt for å lage dynamiske og interaktive nettsider.

## Hvordan

La oss se på et eksempel på hvordan vi kan parse en nettside i Elm og hente ut dataen:

```Elm
import Html
import Html.Attributes exposing (class, src)
import Html.Parser exposing (..)

type alias User = 
    { name : String
    , age : Int
    , country : String
    }

parseUser : Html.Html -> User
parseUser html =
    let
        name =
            html
                |> querySelector ".name"
                |> text

        age =
            html
                |> querySelector ".age"
                |> text
                |> String.toInt
                |> Result.withDefault 0

        country =
            html
                |> querySelector ".country"
                |> text
    in
        User name age country
```

La oss forklare hva som skjer her. Først importerer vi to moduler, `Html` og `Html.Parser`. Deretter definerer vi en type `User` som representerer dataen vi ønsker å hente ut fra nettsiden. I `parseUser` funksjonen vår bruker vi `querySelector` funksjonen fra `Html.Parser` for å få tak i de elementene vi ønsker basert på klassen deres. Deretter bruker vi `text` funksjonen for å hente ut teksten fra disse elementene. Til slutt bruker vi `String.toInt` funksjonen for å konvertere alderen til et heltall.

Vi kan teste denne funksjonen ved å definere en HTML streng med samme struktur som nettsiden vi vil parse, og bruke `parseUser` funksjonen for å få ut dataen. Her er en HTML streng og resultatet fra å parse den:

```Elm
input : String
input =
    """
    <div>
        <h1 class="name">Ola Nordmann</h1>
        <p class="age">30</p>
        <p class="country">Norge</p>
    </div>
    """

result : User
result =
    parseUser (Html.Parser.parse input)

{ name = "Ola Nordmann", age = 30, country = "Norge" } : User
```

Som du kan se får vi ut en `User` med riktig data fra HTML strengen vår. Du kan også prøve å eksperimentere med andre HTML strukturer og se hvordan funksjonen tilpasser seg.

## Dypdykk

Dette var et enkelt eksempel på hvordan man kan parse HTML i Elm, men det finnes mange andre funksjoner og metoder som kan hjelpe deg med å hente ut data. Du kan for eksempel bruke `queryAll` funksjonen for å få tak i alle elementer med en spesifikk klasse, eller bruke `getAttribute` funksjonen for å få tak i verdien av en spesifikk attributt.

Det er også verdt å merke seg at `Html.Parser` modulen er en del av kjernen i Elm og derfor er inkludert i alle prosjekter. Dette gjør det enkelt å implementere parsing av HTML i dine webapplikasjoner uten å måtte legge til eksterne avhengigheter.

## Se også
- [Html.Parser dokumentasjon](https://package.elm-lang.org/packages/elm/html/latest/Html-Parser)
- [Offisiell Elm hjemmeside](https://elm-lang.org/)
- [Elm Diskusjonsforum](https://discourse.elm-lang.org/)