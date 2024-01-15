---
title:                "Merkkijonon kirjoittaminen isolla alkukirjaimella"
html_title:           "Haskell: Merkkijonon kirjoittaminen isolla alkukirjaimella"
simple_title:         "Merkkijonon kirjoittaminen isolla alkukirjaimella"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku kirjoittaisi Haskellilla ja käyttäisi aikaa merkkijonon alkukirjainten muuttamiseen isoiksi? Vaikka tämä saattaa vaikuttaa pieneltä ja yksinkertaiselta tehtävältä, se on hyödyllinen taito monissa ohjelmointiprojekteissa, kuten tekstin käsittelyssä ja valmiiden algoritmien käytössä.

## Kuinka

```Haskell
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
```

Tämän yksinkertaisen funktion avulla voimme muuttaa merkkijonon ensimmäisen kirjaimen isoksi käyttämällä `toUpper` funktiota ja yhdistää sen lopun merkkijonon kanssa käyttäen rekursiota.

Esimerkiksi, suorittamalla `capitalize "haskell"` saamme tulokseksi `"Haskell"`.

## Syvemmälle

Nyt kun olemme luoneet perustoiminnon merkkijonon ensimmäisen kirjaimen muuttamiseen isoksi, voimme ajatella muita tapoja muokata merkkijonoa. Voimme esimerkiksi luoda funktion `titleCase`, joka muuttaa jokaisen merkkijonon sanan ensimmäisen kirjaimen isoksi:

```Haskell
import Data.Char -- `toTitle` funktio on saatavilla Data.Char moduulissa

titleCase :: String -> String
titleCase "" = ""
titleCase (x:xs) = toTitle x : titleCaseHelper xs

-- `titleCaseHelper` funktio auttaa muuttamaan jokaisen sanan ensimmäisen kirjaimen isoksi
titleCaseHelper :: String -> String
titleCaseHelper "" = ""
titleCaseHelper (x:xs)
    | x == ' ' = x : titleCase xs -- lisätään välilyönti ja jatketaan rekursiota
    | otherwise = toLower x : xs -- muutetaan merkki pieneksi ja jatketaan rekursiota
```

Nyt esimerkiksi `titleCase "hello world"` antaa tulokseksi `"Hello World"`.

## Katso myös

- [Data.Char - Haskellin virallinen dokumentaatio](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskellin virallinen verkkosivusto](https://www.haskell.org/)