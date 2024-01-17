---
title:                "Otekirjojen erottaminen"
html_title:           "Elm: Otekirjojen erottaminen"
simple_title:         "Otekirjojen erottaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Substringien erottaminen tarkoittaa osan merkkijonosta erottamista ja käyttämistä erillisenä. Tämä on tärkeää esimerkiksi silloin, kun halutaan käsitellä tiettyä osaa merkkijonosta erikseen. Ohjelmoijat käyttävät substringien erottamista monissa tilanteissa, kuten tietojen käsittelyssä tai käyttöliittymän muokkaamisessa.

## Miten:
```Elm
import String

myString = "Hei kaikille!"
substring = String.slice 4 8 myString

-- Outputs: "kaik"
```
Ohjeessa käytetään `String` moduulia ja sen `slice` funktiota leikkaamaan merkkijonoa. Kolmella parametrilla määritetään leikatun osan alku- ja loppuindeksit. Tulostus näyttää osan merkkijonosta `"Hei kaikille!"` välillä 4-8.

## Syvällinen sukellus:
Substringien käyttö juontaa juurensa 1960-luvun Fortran-ohjelmointikieleen. Elm ei tarjoa valmiita funktioita substringien erottamiseen, mutta `slice` toimii hyvin yksinkertaisissa tapauksissa. Jos tarvitaan monimutkaisempia substringien erottamisia, kannattaa tutkia muita tapoja, kuten `Pattern` tai `Regular Expressions`.

## Katso myös:
- [Elm dokumentaatio](https://elm-lang.org/docs)
- [substring funktio Backus-Naur-notationilla](https://legacy.cs.indiana.edu/libLocal/docs/parsing/means.txt)
- [W3Schools: String slice metodi](https://www.w3schools.com/jsref/jsref_slice_string.asp)