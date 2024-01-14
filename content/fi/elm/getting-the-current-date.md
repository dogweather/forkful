---
title:                "Elm: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi: 
Miksi kukaan haluaisi hakea nykyisen päivämäärän Elm-ohjelmoinnilla? Hyvä kysymys. Syynä voi olla esimerkiksi tarve näyttää nykyinen päivämäärä käyttäjälle tai tallentaa se tietokantaan.

## Miten: 
Hakeminen nykyinen päivämäärä Elm-ohjelmoinnilla on helppoa ja suoraviivaista. Sinun tarvitsee vain käyttää `Date`-moduulia ja sen funktiota `today`. Voit tulostaa päivämäärän konsolille tai tallentaa sen muuttujaan, ja halutessasi muokata sitä lisäämällä tai vähentämällä päiviä tai kuukausia. Alla on muutama esimerkki:

```Elm
import Date exposing (Date, today)

-- Tulostaa nykyisen päivämäärän konsolille
today |> Date.toString |> Html.text 

-- Tallentaa nykyisen päivämäärän muuttujaan
let
  currentDate = today
in
  Html.text (currentDate |> Date.toString)

-- Lisää päiviä nykyiseen päivämäärään ja tulostaa lopputuloksen
today
  |> Date.add 5 "days"
  |> Date.toString
  |> Html.text
```

Tässä on esimerkin tulostama pvm: `2021-11-02`.

## Deep Dive: 
Nykyisen päivämäärän hakeminen Elm-ohjelmoinnilla perustuu `Date`-moduulin tarjoamaan toiminnallisuuteen. Moduulin tärkein funktio on `today`, joka palauttaa tällä hetkellä olevan päivämäärän. Tämän jälkeen voit käyttää muita funktioita, kuten `add` ja `sub` lisätäksesi tai vähentääksesi päiviä tai kuukausia nykyiseen päivämäärään. Muista myös muuttaa päivämäärä haluttuun muotoon `toString`-funktion avulla ennen kuin tulostat sen tai tallennat muuttujaan.

## Katso myös:
- [Elm-ohjelmointikielen kotisivut](https://elm-lang.org/)
- [Virallinen Elm-oppikirja](https://guide.elm-lang.org/)
- [Date-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Date)