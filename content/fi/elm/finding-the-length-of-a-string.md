---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringin pituuden mittaaminen tarkoittaa merkkien lukumäärän laskemista tietyssä merkkijonossa. Ohjelmoijat tekevät tämän määrittääkseen kuinka paljon tilaa (muistissa) merkkijono vie tai tarkistamaan merkkijonon sisällön.

## Näin teet:

Voit laskea merkkijonon pituuden Elm kielellä `String.length` funktiolla. Tässä on yksinkertainen esimerkki:

```Elm
import Html exposing (text)

main =
  let
    s = "Hei maailma"
  in
    text (String.fromInt (String.length s))
```

Koodin suorittamisen tulostuksena saamme:

```
11
```

## Syvällisempi tarkastelu

Elm-kieli on suunniteltu erityisesti turvalliseen etu-pääkoodaukseen ja se on saanut inspiraation Haskell- ja Standard ML -kielistä. `String.length` on suoraan periytynyt näistä funktiopohjaisista kielistä.

Vaihtoehtoisesti, voit käyttää `List.length << String.toList` yhdistelmää, mutta huomaa, että `String.length` on tehokkaampi, koska se ei muunna merkkijonoa listaksi.

Hajotaksemme Elm:n `String.length` toiminnan, se käy läpi merkkijonon ja lisää laskuria jokaista merkkiä kohden, palauttaen lopullisen arvon. Vaikka tämä kuulostaa yksinkertaiselta, sitä se itsekin on!

## Katso myös

Tässä on lisätietoja Elm:n merkkijonoista ja niiden käsittelystä:

1. Elm String data types: https://package.elm-lang.org/packages/elm/core/latest/String
2. Elm Language Guide (Strings): https://guide.elm-lang.org/core_language.html#strings
3. Elm string length function: https://package.elm-lang.org/packages/elm/core/latest/String#length