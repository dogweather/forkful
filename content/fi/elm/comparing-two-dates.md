---
title:                "Elm: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa ohjelmoinnissa joudutaan vertailemaan kahta päivämäärää, kuten päivämäärän muodostamisessa tai tietojen lajittelussa aikajärjestykseen. Elm-kielellä tämä on helppoa ja suoraviivaista, ja tässä blogikirjoituksessa jaamme vinkkejä, kuinka se tehdään.

## Kuinka tehdä

Vertaillessa päivämääriä Elm-kielellä on tärkeää huomioida niiden oikea formaatti ja käyttää sopivia funktioita vertailun suorittamiseen. Alla on muutamia esimerkkejä käyttäen formattausta "RRRR/KK/PP".

```
Elm säädöspäivä: print (Date.fromIsoString "2020/10/15")
Elm vanhempipäivä: print (Date.fromIsoString "2020/10/10")
```

Jos haluamme tarkistaa, kumpi päivämäärä on uudempi, voimme käyttää funktiota `Date.compare`, joka palauttaa `LT` jos ensimmäinen päivämäärä on aikaisempi, `GT` jos toinen päivämäärä on aikaisempi tai `EQ` jos päivämäärät ovat samat.

```
Elm Date.compare Elm päiväratkaisu
ELM Date.Compare Elm asetus# sisältö
```

Voimme myös halutessamme laskea päivämäärien välisen päivien määrän käyttämällä funktiota `Date.diffInDays`.

```
Elm säädöspäivä: print (Date.diffInDays säädöspäivämäärä vanhempipäivämäärä)
```

## Syvällisempi sukellus

Päivämäärien vertaileminen Elm-kielellä perustuu niiden järjestämiseen aikajärjestykseen. Tämä tarkoittaa sitä, että aikaisempi päivämäärä on pienempi ja myöhäisempi päivämäärä on suurempi. Tämä järjestämisen logiikka on hyvä pitää mielessä vertaillessa päivämääriä.

## Katso myös

- [Elm ohjelmointikielen viralliset dokumentaatiot](https://guide.elm-lang.org/)
- [Päivämäärä- ja aikakirjasto Elm-kielellä](https://package.elm-lang.org/packages/elm/time/latest/) 
- [Elixir-ohjelmointikielen vertailu kahden päivämäärän välillä](https://elixir-lang.org/getting-started/basic-types.html#date-and-time)