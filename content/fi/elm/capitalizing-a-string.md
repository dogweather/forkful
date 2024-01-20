---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Isojen kirjainten käyttö stringissä tarkoittaa, että muutetaan kaikki merkit isoiksi kirjaimiksi. Ohjelmoijat tekevät tämän esimerkiksi korostaakseen otsikkoja tai varmistaakseen, että käyttäjän syöte käsitellään yhdenmukaisesti.

## How to:
Elm:ssä stringien käsittely on suoraviivaista. Tässä esimerkki, miten muutetaan merkkijono isoiksi kirjaimiksi:

```Elm
toUpper : String -> String
toUpper str =
    String.toUpper str

main =
    let
        original = "terveisiä suomesta!"
        capitalized = toUpper original
    in
    Html.text capitalized
-- Output: "TERVEISIÄ SUOMESTA!"
```

## Deep Dive
Elm ei sisällä samanlaista `toUpperCase` -funktiota kuin JavaScript, mutta `String.toUpper` ajaa saman asian. Historiallisesti, merkkijonojen muokkaus on ollut tärkeää, koska vanhemmissa järjestelmissä isoilla kirjaimilla oli erilaisia käyttötarkoituksia, kuten komentojen ja nimien erottaminen. Vaihtoehtoisesti, jos haluat vain ensimmäisen kirjaimen isoksi etkä koko stringiä, joudut toteuttamaan sen itse, koska Elm ei tarjoa valmista funktiota sille.

Toinen vaihtoehto isoille kirjaimille on käyttää CSS:tä näyttövaiheessa, mutta Elm:ssä muutetaan merkkijono ohjelmallisesti ennen näyttöä. Suorituskyvyn suhteen stringien muuttaminen isoksi kirjaimin Elm:ssä on yhtä nopeaa kuin missä tahansa muussa modernissa ohjelmointikielessä.

## See Also
Elm String dokumentaatio: [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
Stringin työskentelyn perusteet: [Working with Strings in Elm](https://elmprogramming.com/working-with-strings.html)
Elm opas aloittelijoille: [Elm Guide for Beginners](https://guide.elm-lang.org/)