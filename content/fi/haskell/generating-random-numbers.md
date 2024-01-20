---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi tarkoittaa ennalta arvaamattomien numeroiden luomista ohjelmissamme. Ohjelmoijat tekevät sen monille tarpeille, esimerkiksi ohjelmistojen testaamiselle tai satunnaisen käyttäjäkokemuksen luomiselle.

## Näin tehdään:
Haskellissa voimme generoida satunnaislukuja `randomRIO`:n avulla, joka on osa `System.Random`-kirjastoa. Tässä on esimerkki, joka tuottaa satunnaisen kokonaisluvun väliltä 1-10.

```Haskell
import System.Random

main :: IO ()
main = do
    randomNum <- randomRIO (1,10) :: IO Int
    print randomNum
```

Kun ajat tämän ohjelman, saat tuloksen, joka on satunnaisluku väliltä 1-10.

## Syvempi sukellus:
Satunnaislukugenerointi on ollut olennainen osa ohjelmistoja ja pelejä vuosikymmenten ajan. Haskellissa `randomRIO` käyttää lineaarista kongruenssi-generaattoria, joka on yksinkertainen, muttei täydellinen ratkaisu.

Vaihtoehtoisesti voi käyttää esim. Mersenne Twisteriä, joka on monimutkaisempi, mutta tuottaa "satunnaisempia" lukuja.

Satunnaisuuden todellista tavoittamista pidetään kuitenkin matemattisesti mahdottomana: lähtökohtaisesti kaikki algoritmit toistavat ennustettavan kuvion annetun ajanjakson jälkeen.

## Katso myös:
- [Haskellin virallinen dokumentaatio System.Random-kirjastosta.](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- [Mersenne Twister -algoritmin esite.](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Keskustelu satunnaislukugeneraattoreiden luonteesta.](https://stackoverflow.com/questions/39853457/how-to-generate-random-float-number-in-haskell)