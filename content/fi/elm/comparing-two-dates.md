---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Vertaamme kahta päivämäärää tarkistaksemme, kumpi on varhaisempi tai myöhäisempi. Tämän tiedon avulla voidaan esimerkiksi järjestää tapahtumia ajoissa tai seurata vanhenemisia.

## Miten:
Elm tarjoaa `Date.compare` -funktion, jolla voimme vertailla päivämääriä. 

```elm
import Date exposing (fromString, compare)
import Result exposing (withDefault)

date1 = withDefault (Date.fromTime 0) (fromString "2021-01-01")
date2 = withDefault (Date.fromTime 0) (fromString "2021-12-01")

main =
    compare date1 date2
```

Tämän koodin tuloste on `LT`, mikä tarkoittaa, että `date1` on varhaisempi kuin `date2`. 

## Syvä sukellus:
`Date.compare` -funktio on osa Elm:n ydinkirjastoa. Se käyttää aikaleimoja (millisekunteja vuodesta 1970) päivämäärien vertaamiseen. Vaihtoehtoinen tapa päivämäärien vertaamiseen on muuntaa ne ensin aikaleimoiksi ja verrata sen jälkeen tätä arvoa.


## Katso myös: 
Kannattaa tutustua näihin lähteisiin, jos haluat lisätietoja päivämäärien vertaamisesta Elm:ssä.

- Elm:n virallinen [Date](https://package.elm-lang.org/packages/elm/core/latest/Date) moduuli.
- Tietoa [aikaleimoista](https://en.wikipedia.org/wiki/Timestamp) ja niiden merkityksestä datan vertaamisessa.
- Stack overflow opas päivämäärien [vertaamisesta](https://stackoverflow.com/questions/3708355/compare-dates-in-javascript) muissa ohjelmointikielissä, kuten JavaScript.