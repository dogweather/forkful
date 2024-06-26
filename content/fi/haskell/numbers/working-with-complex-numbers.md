---
date: 2024-01-26 04:41:23.304358-07:00
description: "Kuinka: Haskell k\xE4sittelee kompleksilukuja `Data.Complex`-moduulin\
  \ avulla. T\xE4ss\xE4 on pikakierros."
lastmod: '2024-03-13T22:44:56.609571-06:00'
model: gpt-4-0125-preview
summary: "Haskell k\xE4sittelee kompleksilukuja `Data.Complex`-moduulin avulla."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
Haskell käsittelee kompleksilukuja `Data.Complex`-moduulin avulla. Tässä on pikakierros:

```haskell
import Data.Complex

-- Määritellään kaksi kompleksilukua
let z1 = 3 :+ 4  -- eli 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Aritmeettiset operaatiot
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Kompleksikonjugaatti
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- Suuruus ja vaihe
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Polaarinen -ja suorakaidekoordinaattien muunnos ja päinvastoin
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- sama kuin z1
```

Esimerkkituloste koodin lataamisen jälkeen GHCi:ssä voisi olla:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## Syväsukellus
Kompleksiluvut ulottuvat 16. vuosisadalle, mutta ne hyväksyttiin laajalti paljon myöhemmin. Haskell, kuten monet muut kielet, tarjoaa natiivituen kompleksiaritmetiikalle, mikä tekee näiden lukujen kanssa työskentelystä helppoa ilman perustavanlaatuisen matematiikan toteuttamista.

Vaihtoehtoihin kuuluu oman kompleksilukutyypin rakentaminen tai erityisille aloille, kuten quaternionit 3D-grafiikkaan, tarkoitettujen kirjastojen käyttäminen. Mutta useimmissa käyttötapauksissa Haskellin `Data.Complex` riittää.

`Data.Complex`in alla on vain tietotyyppi, joka parittaa kaksi `Float` tai `Double` -arvoa, edustaen vastaavasti reaali- ja imaginääriosia. Se on suoraviivainen ja tehokas tapa työskennellä kompleksilukujen kanssa Haskell-alustalla.

## Katso myös
Tutustu näihin resursseihin saadaksesi lisätietoja kompleksiluvuista Haskellissa:

- Virallinen Haskell `Data.Complex` dokumentaatio: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Syvempi sukellus Haskellin numeerisiin tyyppeihin: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- Sovellusesimerkkinä, tutustu Fast Fourier Transform algoritmeihin Haskellissa: [Haskell FFT kirjasto](https://hackage.haskell.org/package/fft)
