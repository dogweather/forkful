---
title:    "Haskell: Merkkijonon isojen kirjainten muuttaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi stringin isojentaminen (capitalize) on hyödyllinen ohjelmoinnin työkalu?

Isolla alkukirjaimella kirjoitetut stringit ovat monen ohjelmointikielen tärkeä osa, ja ne auttavat lukemaan ja ymmärtämään koodia. Isolla alkukirjaimella kirjoitetut sanat tai lauseet ovat myös helpompi havaita ja erottaa lukujonosta.

## Kuinka: Näin isojennat stringin Haskell-ohjelmointikielellä

```Haskell
import Data.Char

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
```

```Haskell
decapitalized = "tämä on pienellä alkukirjaimella"
capitalized = capitalize decapitalized
```

Tämä koodi ottaa vastaan stringin ja palauttaa sen isollaan. Käytämme ensin `import Data.Char` komentoa, jotta voimme käyttää `toUpper` funktiota. Sitten määrittelemme `capitalize` funktion, joka ottaa vastaan stringin muodossa `(x:xs)`. Tämä tarkoittaa, että funktio ottaa ensimmäisen kirjaimen `x` ja loput kirjaimet `xs`. Käytämme sitten `toUpper` funktiota ensimmäiselle kirjaimelle ja liitämme sen loppuun alkuperäiset kirjaimet `xs`. Lopuksi voimme käyttää `capitalize` funktiota missä tahansa stringissä saadaksesi sen isoksi.

## Syväluotaus: Tarkempaa tietoa stringin isojentamisesta

Haskellissa on monia tapoja isojentaa string. Voimme käyttää myös funktiota `map toUpper`, joka ottaa vastaan luettelon ja palauttaa sen isolla. Tai voimme käyttää guard-lausuntoja ja pattern matchingia määrittelemään erilaisia tapauksia, joissa haluamme isojentaa stringiä.

Isolla alkukirjaimella kirjoitettu stringi voi myös auttaa meitä tarkistamaan halutun tuloksen oikeellisuuden. Esimerkiksi, jos ohjelmassa on tarkoitus tulostaa nimi isolla kirjoitettuna, niin sen nimen pitäisi olla myös isolla kirjoitettuna koodissa. Jos huomaamme, että nimi ei ole isolla kirjoitettuna, tiedämme, että jokin on vialla koodissa.

## Katso myös

- [Haskell virallinen sivusto](https://www.haskell.org/)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)