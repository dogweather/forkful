---
title:                "Substringien erottaminen"
html_title:           "Haskell: Substringien erottaminen"
simple_title:         "Substringien erottaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan halunnut leikata osia merkkijonoista, mutta et tiennyt kuinka? Hyppää mukaan, sillä tänään opimme miten erottaa kauniisti ja tehokkaasti!

## Miten tehdä niin

```Haskell

-- Luodaan funktio, joka leikkaa alusta halutun määrän merkkejä
otsikko :: String -> String
otsikko x = take 10 x -- Voit vaihtaa 10 haluamaksesi määräksi

-- Käytetään funktiota
otsikko "Tämä on esimerkki"
--Tämä on esi

-- Luodaan funktio, joka leikkaa lopusta halutun määrän merkkejä
loppu :: String -> String
loppu x = reverse (take 10 (reverse x))

-- Käytetään funktiota
loppu "Tämä on esimerkki"
--i on esimer
```

## Syvä sukellus

On hyödyllistä käyttää `take` ja `drop` funktioita leikkaamaan osia merkkijonoista, mutta nämä eivät anna täydellistä hallintaa leikattavan osan sijainnista. Haskell tarjoaa myös muita tehokkaita työkaluja, kuten `splitAt` ja `substring` funktioita, jotka antavat lisää joustavuutta leikkausprosessissa. Muista myös tarkistaa `Data.Text` kirjasto, joka tarjoaa erinomaisia toimintoja merkkijonojen käsittelyyn.

## Katso myös

- [Aloittelijan opas Haskelliin](https://wiki.haskell.org/Introductory_Haskell)
- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation)
- [Haskelliin tutustuminen FUNctionalDELIGHT-kirjan avulla](https://www.cs.nott.ac.uk/~pszgmh/pih.html)