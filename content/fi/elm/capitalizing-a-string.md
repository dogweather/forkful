---
title:                "Elm: Itsekirjoittavan merkkijonon nimeäminen"
simple_title:         "Itsekirjoittavan merkkijonon nimeäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Joskus ohjelmoinnissa tarvitsemme muuttaa merkkijonojen kirjainten suuruutta ja silloin käytetään usein funktiota, joka muuntaa pienet kirjaimet isoiksi.

## Miten
```Elm
capitalize : String -> String
capitalize s =
  String.toUpper s
```
Tämä funktio ottaa merkkijonon ja palauttaa saman merkkijonon, mutta jossa kirjainkoko on muutettu isoiksi.

Esimerkiksi:
```
capitalize "moi" --> "MOI"
capitalize "Elämä on kaunista" --> "ELÄMÄ ON KAUNISTA"
```

## Syväsukellus
Funktion toiminnan ymmärtämiseksi on hyvä tietää pari asiaa merkkijonoista Elm-kielessä. Ensinnäkin, merkkijonot eivät ole muokattavissa, joten kun muutamme kirjainkokoa, meidän täytyy palauttaa uusi merkkijono. Toiseksi, funktio `String.toUpper` ottaa merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat isoja.

## Katso myös
- [Elm-kielessä käytetyt merkkijono-funktiot](https://guide.elm-lang.org/strings/)
- [Tarkempi selitys merkkijonon muokkaamisesta Elm-kielessä](https://jakegoulding.com/blog/2014/12/01/string-explained/)