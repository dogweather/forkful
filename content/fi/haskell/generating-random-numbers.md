---
title:    "Haskell: Sattumanvaraisten numeroiden luominen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Miksi Generoida Satunnaisia Numeroita?

Satunnaisuus on tärkeä osa monia ohjelmointitehtäviä, kuten simulaatioita tai pelien kehitystä. Satunnaisilla numeroilla voidaan luoda monimutkaisia algoritmeja ja erilaisia käyttäytymismalleja. Haskell tarjoaa helpon ja tehokkaan tavan generoida satunnaisia numeroita, jotka voit sisällyttää omiin ohjelmiisi.

## Näin Teet Sen

Haskellissa satunnaisia numeroita voidaan generoida käyttämällä `System.Random` -kirjastoa. Ensimmäiseksi tarvitset `RandomGen` -luokan ilmentymän, joka on ohjelmalla luotu siemeniä varten. Tämän jälkeen voit käyttää `random`-funktiota, joka palauttaa satunnaisen numeron annetulla välillä. Alla on yksinkertainen esimerkki, jossa generoidaan 10 satunnaista kokonaislukua välillä 0-100.

```Haskell
import System.Random
  
main = do
  -- Luodaan satunnaisgeneraattori
  gen <- getStdGen
  -- Generoidaan 10 satunnaista kokonaislukua
  let numbers = take 10 $ randomRs (0, 100) gen :: [Int]
  -- Tulostetaan saadut numerot
  putStrLn $ "Satunnaiset numerot: " ++ show numbers
```

Ohjelman tulostama teksti voi näyttää tältä:

```
Satunnaiset numerot: [56, 83, 27, 91, 37, 69, 2, 18, 99, 73]
```

Voit myös asettaa oman siemenen `mkStdGen` -funktiolla, jolloin voit toistaa samat satunnaiset numerot myöhemmin. Esimerkiksi `mkStdGen 42` luo satunnaisgeneraattorin, joka tuottaa aina samat numerot.

## Syvempi Sukellus

Satunnaislukujen generointi Haskellissa perustuu lineaariseen kongruenssimenetelmään. Tämä on yksinkertainen matemaattinen algoritmi, joka tuottaa pseudo-satunnaisia lukuja tietyn siemenarvon perusteella. Jos haluat enemmän tietoa tähän menetelmään liittyen, voit tutustua esimerkiksi [Wikipedia-artikkeliin](https://en.wikipedia.org/wiki/Linear_congruential_generator).

Haskellissa on myös muita tapoja generoida satunnaisia arvoja, kuten `Gen`-monadiksi nimitetty kirjasto. Tämä mahdollistaa esimerkiksi satunnaisten merkkijonojen ja rakenteiden generoinnin. Voit lukea lisää tästä [Haskellin dokumentaatiosta](https://hackage.haskell.org/package/gen).

# Katso Myös

- [Haskell dokumentaatio - System.Random](https://hackage.haskell.org/package/random)
- [Wikipedia - Pseudosatunnainen luku](https://fi.wikipedia.org/wiki/Pseudosatunnainen_luku)