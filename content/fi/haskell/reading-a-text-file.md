---
title:                "Haskell: Tiedoston lukeminen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi Lukea Tekstitiedosto?

Tekstitiedostoja käytetään laajasti tietojen tallentamiseen ja jakamiseen eri ohjelmien välillä. Tekstitiedoston lukeminen on tärkeä taito, joka mahdollistaa tietojen käsittelyn ja muokkaamisen ohjelmoinnin avulla.

## Miten Teet Sen?

Haskell tarjoaa helpon tavan lukea tekstitiedostoja käyttämällä funktiota nimeltä "readFile". Tämä funktio ottaa parametrina tiedoston polun ja palauttaa tekstitiedoston sisällön merkkijonona.

```Haskell
teksti <- readFile "tiedosto.txt"
print teksti
```

Tämä koodinpätkä lukee tiedoston nimeltä "tiedosto.txt" ja tallentaa sen sisällön muuttujaan "teksti". Sitten se tulostaa tekstin konsoliin.

## Syventävä sukellus

Tekstitiedostojen lukeminen ei rajoitu vain yksinkertaisiin merkkijonoihin, vaan Haskellilla on monipuolisia tapoja käsitellä monimutkaisempia tiedostoja. Esimerkiksi "Data.Text" -kirjasto tarjoaa tehokkaan tavan käsitellä tekstiä ja suorittaa erilaisia operaatioita tekstitiedostojen kanssa.

```Haskell
import Data.Text as T

tekstit <- T.readFile "tiedosto.txt"
let rivit = T.lines tekstit
let pituudet = T.length <$> rivit
print pituudet
```

Tässä esimerkissä käytetään "Data.Text" -kirjastoa lukemaan tiedosto ja tallentamaan sen rivit listaan. Sitten käytetään "T.length" -funktiota laskemaan jokaisen rivin pituus ja tulostetaan lopuksi lista pituuksista.

## Katso myös

- [Haskellin virallinen dokumentaatio tekstitiedostojen lukemisesta](https://www.haskell.org/documentation)
- [Data.Text-kirjaston dokumentaatio](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [Haskell-oppaat ja tutoriaalit tekstitiedostojen käsittelyyn](https://github.com/pulled-muscle/pyshootout/wiki/Tutorials)