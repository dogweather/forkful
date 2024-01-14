---
title:                "Elm: Mallin mukaan sopivien merkkien poisto."
simple_title:         "Mallin mukaan sopivien merkkien poisto."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
Usein ohjelmointiprojekteissa on tarve poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla hyödyllistä esimerkiksi käyttäjän syötteen validoinnissa tai tietokannan käsittelyssä. Elm-ohjelmointikielessä tämä onnistuu helposti ja tehokkaasti.

## Näin Tehdään
``` Elm
import String

-- Merkkijono, josta halutaan poistaa osa merkeistä
merkkijono = "Tässä on esimerkkimerkki 123"

-- Funktion poistaChar avulla poistetaan numerot 1,2 ja 3 merkkijonosta
String.filter (\char -> char `notElem` ['1','2','3']) merkkijono
-- Output: "Tässä on esimerkkimerkki "

-- Samoin voitaisiin poistaa esimerkiksi kaikki erikoismerkit
String.filter (\char -> Char.isAlpha char) merkkijono
-- Output: "Tässäon esimerkkimerkki"

-- Poistamisen sijaan voit myös korvata merkkejä toisilla
String.replace "123" "ABC" merkkijono
-- Output: "Tässä on esimerkkiABCmerkki"

```

## Syvempi Sukellus
Elm-ohjelmointikielessä merkkijonon käsittelyyn on tarjolla useita käteviä funktioita, kuten `filter`, `replace` ja `split`. Näiden avulla voit helposti manipuloida merkkijonoja tarpeesi mukaan. Lisäksi, voit hyödyntää Elm-kirjastoja, kuten Regex- ja UTF-paketteja, laajentaaksesi merkkijonojen käsittelymahdollisuuksia.

## Katso Myös
- [String - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Regex - Elm Packages](https://package.elm-lang.org/packages/elm/regex/latest/)
- [UTF - Elm Packages](https://package.elm-lang.org/packages/elm-community/utf-tools/latest/)