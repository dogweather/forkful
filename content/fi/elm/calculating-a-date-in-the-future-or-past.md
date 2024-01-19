---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Elm: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa tietyn ajanjakson lisäämistä tai vähentämistä jonkin tietyn päivämäärän päälle. Ohjelmoijat tekevät sen ajastamaan toimintoja, hallitsemaan tapahtumia tai laskemaan päiviä erityisten päivämäärien välillä.

## Miten tehdään:

Koodaus esimerkki, jossa luodaan uusi päivämäärä seuraavalle päivälle Elm:n avulla:

```Elm
import Time exposing (..)

saatUudenPaivamaaran : Posix -> Posix
saatUudenPaivamaaran vanhaPaivamaara =
    addDays 1 vanhaPaivamaara

-- Käyttöesimerkki
uusiPaivamaara = saatUudenPaivamaaran (fromMillis (toFloat (Date.now())))
```
Output saattaa näyttää tältä:
```Elm
Posix 1584662400000 -- Seuraavan päivän Unix-aika
```

## Syvä sukellus:

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on ollut ohjelmoinnin osa niin kauan kuin ihmiset on tarvinnut ajastaa toimintaansa. Elm tarjoaa `Time` moduulin tarkkaan päivämäärän käsittelyyn.

Vaihtoehtoina ovat muun muassa JavaScriptin sisäänrakennetut Date-objektit tai jos tarvitset lisää toiminnallisuuksia, voit käyttää `elm-date-extra`-kirjastoa.

Lasketaan päivämäärä lisäämällä tai vähentämällä millisekunteja. Elm hoitaa yksityiskohdat, kuten karkausvuodet ja eri kuukausien päivien määrät, puolestamme.


## Katso myös:

- Elm:n virallinen [Time](https://package.elm-lang.org/packages/elm/time/latest/) dokumentaatio
- Erinomainen [elm-date-extra](https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest) kirjasto päivämäärätoiminnoille