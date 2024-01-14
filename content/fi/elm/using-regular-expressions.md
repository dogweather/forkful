---
title:    "Elm: Säännöllisten lausekkeiden käyttö"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressionit ovat tärkeä osa monen ohjelmointikielen työkalupakkia, myös Elm-kielellä. Niitä käytetään etsimään, korvaamaan ja muokkaamaan tekstiä helposti ja tehokkaasti.

## Kuinka käytät regular expressioneja Elm-ohjelmoinnissa

Regular expressioneja käytetään erilaisissa ohjelmointitehtävissä, kuten tekstin validoinnissa, hakemisessa ja korvaamisessa. Näitä tehtäviä varten Elm tarjoaa RegularExpressions-moduulin, jonka voit tuoda käyttöösi yläpuolella olevalla import-lauseella.

```Elm
import RegularExpressions exposing (..)
```

Seuraavaksi voit käyttää moduulin tarjoamia funktioita luodaksesi regular expressionin, etsiäksesi sitä tekstistä ja käsitelläksesi tuloksen. Alla on esimerkki tekstin validoinnista, jossa tarkistetaan, onko syötetty sähköpostiosoite oikeassa muodossa.

```Elm
validoiSähköposti : String -> Bool
validoiSähköposti sähköposti =
    let
        regex = RegExp.fromString "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$"
    in
        case regex of
            Ok r -> 
                case RegExp.find r sähköposti of
                    Just _ -> True
                    Nothing -> False
            Err _ -> False
```

Yllä oleva koodi käyttää RegularExpressions-moduulin `find`-funktiota etsimään sähköpostiosoitetta vastaavan regular expressionin. Jos osoite löytyy, funktio palauttaa `Just`-arvon, muussa tapauksessa `Nothing`-arvon.

## Syventävä sukellus

Vaikka regular expressioneja voidaan käyttää moniin eri tehtäviin, niiden käyttö voi olla aluksi haastavaa ja hämmentävää. Onkin tärkeää ymmärtää perusteet, kuten mitä merkityksiä eri erikoismerkit ja metakarakterit kuten `^`, `$` ja `+` tarkoittavat.

On myös hyvä huomata, että regular expressioneilla on erilaisia syntaksimuotoja eri ohjelmointikielissä. Siksi kannattaa tutustua tarkasti Elm-kielellä käytettyyn syntaksiin.

## Katso myös

- [Elm RegularExpressions-moduuli](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Regular expressionien opas](https://www.regular-expressions.info/elm.html)
- [Elm-ohjelmointikielen virallinen sivusto](https://elm-lang.org/)