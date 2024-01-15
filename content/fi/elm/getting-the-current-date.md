---
title:                "Päivämäärän hakeminen"
html_title:           "Elm: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi?

Nykyisen päivämäärän saaminen voi olla hyödyllistä monissa sovelluksissa, kuten tapahtumakalentereissa, muistutussovelluksissa ja ajanhallintatyökaluissa.

## Kuinka tehdä?

```Elm
import Time exposing (Date, Time)
import Date exposing (today)

-- Hae nykyinen päivämäärä
currentDate : Date
currentDate =
    today

-- Tulosta päivämäärä konsoliin
main : Program () Date Never
main =
    currentdate
        |> toString
        |> Html.text
        |> Html.program ()

```

**Tulostaa:** "2021-10-25"

## Syvällinen sukellus

Nykyisen päivämäärän saamiseen käytetään Elm-ydinkirjaston `Date` ja `Time` moduuleja. `today`-funktio palauttaa nykyisen päivämäärän `Date`-tyyppisenä arvona. Tämä arvo voidaan sitten muuntaa halutunlaiseen muotoon esimerkiksi `toString`-funktion avulla.

## Katso myös

- [Date - Elm Documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Time - Elm Documentation](https://package.elm-lang.org/packages/elm/time/latest/Time)