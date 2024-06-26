---
date: 2024-01-20 17:45:23.694495-07:00
description: "How to: - Kuinka tehd\xE4: N\xE4m\xE4 esimerkit k\xE4ytt\xE4v\xE4t `String.slice`\
  \ funktiota, joka ottaa alku- ja loppuindeksit sek\xE4 merkkijonon ja palauttaa\
  \ osajonon."
lastmod: '2024-04-05T22:38:57.080278-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4: N\xE4m\xE4 esimerkit k\xE4ytt\xE4v\xE4t `String.slice`\
  \ funktiota, joka ottaa alku- ja loppuindeksit sek\xE4 merkkijonon ja palauttaa\
  \ osajonon."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: - Kuinka tehdä:
```Elm
import String

-- Ota osajono merkkijonosta ‘hello world’ indeksistä 0 merkkien määrällä 5.
substringExample : String
substringExample =
    String.slice 0 5 "hello world"
-- Tämä antaa tulokseksi “hello”

-- Ota osajono ‘programming is fun’ indekseistä 0 to 11.
anotherExample : String
anotherExample =
    String.slice 0 11 "programming is fun"
-- Tämä antaa tulokseksi "programming"
```
Nämä esimerkit käyttävät `String.slice` funktiota, joka ottaa alku- ja loppuindeksit sekä merkkijonon ja palauttaa osajonon.

## Deep Dive - Syväsukellus:
Substringien poiminnan tarve tuli alkuaikoina, kun tiedon käsittely ja tekstin manipulointi olivat ohjelmoinnin ydintehtäviä. Elm:ssä `String.slice` on moderni tapa käsitellä osajonoja, mutta kielessä on muitakin tapoja, kuten `String.left` ja `String.right`, joilla voidaan ottaa osajonoja kiinteiden indeksien mukaan. Elm:ssä osajonon poiminnan suorituskyky riippuu kohdejonojen koosta ja käytetystä selaimesta, koska Elm käyttää selaimen JavaScript-ympäristön String-objektia prosessoinnissa.

## See Also - Lisäksi:
- Elm String dokumentaatio: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Interaktiivinen Elm repl kokeiluihin: [https://elm-lang.org/try](https://elm-lang.org/try)
- Elm opetusta: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
