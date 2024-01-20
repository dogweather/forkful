---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstinhaku ja -korvaus ovat ohjelmoinnissa käytettyjä toimintoja, joissa etsitään tiettyjä merkkijonoja ja korvataan ne toisella merkkijonolla. Ohjelmoijat käyttävät tätä toimintoa usein datan muokkaamiseen tai virheiden korjaamiseen koodissa.

## Näin se tehdään:

Alla on koodiesimerkki Elm-ohjelmointikielellä. Esimerkki näyttää kuinka voit etsiä ja korvata merkkijonoja.

```Elm
module Main exposing (..)

import String

replace :: String -> String -> String -> String
replace old new = 
    String.split old >> String.join new 

main =
    "Hello, World!" |> replace "World" "Finland"
```

Esimerkkikoodin tuloste on:

```Elm
"Hello, Finland!"
```

## Syvempi tarkastelu:

Tekstinhaku ja -korvaus on hyvin vanha ohjelmointitemppu. Sen jäljet kulkevat aina 1970-luvulle, jolloin ensimmäiset tekstinkäsittelyjärjestelmät esiteltiin.

Elm tarjoaa `String.split` ja `String.join` integroidun työkalun tekstinhakuun ja -korvaukseen, mutta on olemassa myös muita tapoja tehdä tämä. Voit esimerkiksi käyttää regex-syntejä, jos sinun on käsiteltävä monimutkaisempia kuvioita.

Elm:ssä `String.split` funktio jakaa merkkijonon haluttujen symbolien tai merkkijonojen mukaan palauttaen taulukon merkkijonoja. `String.join` funktio toimii päinvastoin, se liittää merkkijonot yhteen käyttäen erottimena annettua merkkijonoa.

## Katso myös:

Lisää esimerkkejä ja tietoa String.split ja String.join -funktioista Elm-ohjelmointikielessä löydät täältä:

- [Elm String.join dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String#join)
- [Elm String.split dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String#split)