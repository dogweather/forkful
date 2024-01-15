---
title:                "Satunnaislukujen luominen"
html_title:           "Haskell: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Joku voi haluta luoda satunnaisia numeroita esimerkiksi pelien ja simulaatioiden luomiseen. Satunnaiset numerot voivat myös auttaa testaamaan ja vertaamaan algoritmeja ja ohjelmia.

## Miten
```
Prelude> import Random
Prelude Random> randomRIO (1,10)
4
```
Yllä olevassa esimerkissä käytetään `randomRIO`-funktiota luomaan satunnainen luku väliltä 1-10. Tämä funktio hyödyntää `IO`-monadia, joka mahdollistaa satunnaisen numeron generoinnin. Voit myös käyttää `randomR`-funktiota, joka palauttaa arvon `RandomGen`-tyyppisenä.

```
Prelude> import System.Random
Prelude System.Random> randomR (1,10) (mkStdGen 42)
(5, StdGen 1095936233 363907491)
```
Yllä olevassa esimerkissä käytetään `randomR`-funktiota luomaan satunnainen luku väliltä 1-10. `mkStdGen`-funktio luo uuden satunnaisgeneraattorin arvon 42 perusteella.

## Syväsukellus
Satunnaisia numeroita generoidaan käyttäen satunnaistusfunktioita, jotka pohjautuvat erilaisiin algoritmeihin. Haskellissa on valmiiksi määriteltyjä satunnaistusfunktioita, kuten `randomR` ja `randomRIO`, jotka perustuvat Mersenne Twister -generaattoriin.

Voit myös luoda omia satunnaistusfunktioita käyttämällä `RandomGen`-tyyppiä ja sen funktioita, kuten `next` ja `split`. Tämä mahdollistaa tarkemman ja monipuolisemman satunnaisen numeron luomisen.

## Katso myös
- [Haskellin dokumentaatio satunnaistusfunktioista](https://hackage.haskell.org/package/random)
- [Haskell kirja: Satunnaislukujen tuottaminen](http://learnyouahaskell.com/input-and-output)
- [Haskellin ohjelmointiopas](https://www.haskell.org/)