---
title:                "Merkkijonon interpolointi"
html_title:           "Elm: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringin interpolointi tarkoittaa muuttujan arvon lisäämistä merkkijonoon. Tämä on kätevä tapa yhdistää dynaamisia arvoja staattisiin merkkijonoihin. Ohjelmoijat käyttävät sitä lisäämään muuttuvuutta ja joustavuutta ohjelmien luomiseen.

## Miten?

Elm-kielessä stringin interpolointi tehdään käyttämällä `String.format` -funktiota, joka ottaa vastaan merkkijonon ja listan arvoja, jotka halutaan lisätä siihen. Alla on esimerkki käytöstä:

```Elm
String.format "Tervehdys %s!" ["maailma"] 
-- Output: "Tervehdys maailma!" 
```

Voit myös käyttää `%d` merkkiä lisätäksesi numeron merkkijonoon, kuten seuraavassa esimerkissä:

```Elm
String.format "Minulla on %d omenaa." [5] 
-- Output: "Minulla on 5 omenaa." 
```

## Syväsukellus

Stringin interpolointi on yleinen tekniikka monissa ohjelmointikielissä. Se antaa mahdollisuuden luoda joustavia ja dynaamisia merkkijonoja, jotka voivat sisältää vaihtelevia arvoja. Tämä vähentää tarvetta staattisten merkkijonojen luomiseen ja lisää mahdollisuuksia muokata ja muuttaa tekstin sisältöä helposti.

On olemassa myös muita tapoja lisätä muuttuvia arvoja merkkijonoon, kuten käyttämällä muotoilumuotoilua tai stringien konkatenointia. Näihin vaihtoehtoihin voi tutustua lisää Elm-dokumentaatiossa.

## Katso myös

- [Elm-dokumentaatio stringin interpoloinnista](https://package.elm-lang.org/packages/elm/core/latest/String#format)
- [Elm-dokumentaatio stringien muotoilumuotoilusta](https://package.elm-lang.org/packages/elm/core/latest/String#format1)
- [Elm-dokumentaatio stringien konkatenoinnista](https://package.elm-lang.org/packages/elm/core/latest/String#concat)