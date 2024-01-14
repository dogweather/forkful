---
title:                "Haskell: Sattumanvaraisten numeroiden generointi"
simple_title:         "Sattumanvaraisten numeroiden generointi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmoijat voivat käyttää satunnaislukuja monissa eri ohjelmoinnin osa-alueissa, kuten peleissä, simuloinneissa ja salausalgoritmeissa.

## Miten

Satunnaislukujen generoiminen on helppoa Haskellissa, sillä siinä on valmiina funktio `random` ja `randomR`, jotka generoivat satunnaislukuja eri tavoin.

```Haskell
import System.Random

-- Generoi satunnaisluku väliltä 1-10.
randomNumber1 :: Int
randomNumber1 = randomRIO (1, 10)

-- Generoi satunnaisluku väliltä 50-100.
randomNumber2 :: Int
randomNumber2 = randomR (gRandom 50 100)

-- Generoi satunnainen True tai False arvo.
randomBool :: Bool
randomBool = random gBool
```

```
>>> randomNumber1
7
>>> randomNumber2
78
>>> randomBool
False
```

## Syventyvä tarkastelu

Haskellissa satunnaislukujen generointi perustuu oletuksena "satunnaisgeneraattoriin" (random number generator), joka ottaa sisäänsä "siemenarvon" (seed), josta se generoi sarjan satunnaislukuja. Sama siemenarvo tuottaa aina saman sarjan satunnaislukuja, joten oikeassa käytössä siemenarvo tulisi vaihtaa joka kierroksella, esimerkiksi käyttäjän antaman satunnaisen arvon pohjalta.

Lisäksi Haskellissa on mahdollista käyttää myös puhtaampaa satunnaislukugeneraattoria, jonka avulla voidaan tuottaa satunnaislukuja funktionaalisen ohjelmoinnin periaatteiden mukaisesti.

## Katso myös

- [Haskellin virallinen dokumentaatio satunnaislukujen generoinnista](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/random/System-Random.html)
- [Ohjeita satunnaislukujen generointiin Haskellissa](http://www.csee.umbc.edu/~olano/s2002c36/ch06.1.pdf)