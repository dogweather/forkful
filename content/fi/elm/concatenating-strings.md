---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Elm: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluatte yhdistää merkkijonoja ohjelmoinnissa? On monia tapoja käyttää ja hyödyntää merkkijonojen yhdistämistä, kuten esimerkiksi tekstin muotoilussa tai tietokantojen käsittelyssä.

## Kuinka

```Elm
concatenateStrings : String -> String -> String
concatenateStrings str1 str2 =
  str1 ++ " " ++ str2

print (concatenateStrings "Moi" "maailma!")

--Tulostaa: "Moi maailma!"
```

Merkkijonojen yhdistäminen Elm-kielellä on helppoa ja intuitiivista. Käytetään yhteenlaskuoperaattoria `++` ja välimerkkiä `" "` yhdistämään merkkijonoja. Tulostus tapahtuu käyttämällä `print` funktiota ja antamalla sille yhdistetty merkkijono parametrina.

## Syvempi sukellus

Merkkijonojen yhdistäminen on usein välttämätöntä silloin, kun halutaan luoda dynaamisia, muuttuvia tekstejä. Esimerkiksi, jos sinulla on käyttäjän syöttämä sana tai lause, voit käyttää merkkijonojen yhdistämistä lisätäksesi sen osana isompaa merkkijonoa.

```Elm
validateUserInput : String -> String
validateUserInput str =
  "Käyttäjä syötti: " ++ str

print (validateUserInput "Hei maailma!")

--Tulostaa: "Käyttäjä syötti: Hei maailma!"
```

Merkkijonoja voi yhdistää myös usealla eri tavalla. Voit käyttää `concat` funktiota tai käyttää `++` operaattoria useita kertoja:

```Elm
concat : List String -> String
concat strList =
  List.foldl (++) "" strList

concatMultiple : String -> String -> String -> String
concatMultiple str1 str2 str3 =
  str1 ++ str2 ++ str3
```

Tutustu myös `String.join` ja `String.append` funktioihin saadaksesi lisätietoa merkkijonojen yhdistämisestä mielenkiintoisilla tavoilla.

## Katso myös

* Elm Language - yksinkertainen ja looginen funktionaalinen ohjelmointikieli: https://elm-lang.org/
* String documentointi Elm-sivustolla: https://package.elm-lang.org/packages/elm/core/latest/String
* Elm Playground - kokeile Elm-koodin kirjoittamista selaimessa: https://elm-lang.org/try