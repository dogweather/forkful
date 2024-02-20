---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:35.512437-07:00
description: "Complexe getallen, bestaande uit een re\xEBel en een imaginair deel,\
  \ zijn essentieel in verschillende computationele velden zoals engineering, natuurkunde\
  \ en\u2026"
lastmod: 2024-02-19 22:05:09.916047
model: gpt-4-0125-preview
summary: "Complexe getallen, bestaande uit een re\xEBel en een imaginair deel, zijn\
  \ essentieel in verschillende computationele velden zoals engineering, natuurkunde\
  \ en\u2026"
title: Werken met complexe getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Complexe getallen, bestaande uit een reëel en een imaginair deel, zijn essentieel in verschillende computationele velden zoals engineering, natuurkunde en signaalverwerking. Programmeurs gebruiken ze om vergelijkingen op te lossen die niet met reële getallen kunnen worden opgelost, zoals het vinden van de wortels van negatieve getallen.

## Hoe:

Haskell gaat om met complexe getallen met de module `Data.Complex`. Hier is een snelle rondleiding:

```haskell
import Data.Complex

-- Definieer twee complexe getallen
let z1 = 3 :+ 4  -- dat is 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Rekenkundige bewerkingen
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Complex toegevoegde
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- Magnitude en fase
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Omzetting van polair naar rechthoekig en vice versa
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- hetzelfde als z1
```

Voorbeelduitvoer na het laden van bovenstaande code in GHCi zou kunnen zijn:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## Diepere Duik

Complexe getallen gaan terug tot de 16e eeuw maar werden veel later algemeen aanvaard. Haskell, net als veel talen, biedt inheemse ondersteuning voor complexe rekenkunde, waardoor het gemakkelijk is om met deze getallen te werken zonder de onderliggende wiskunde te implementeren.

Alternatieven zijn het bouwen van je eigen complexe getaltype of het gebruiken van bibliotheken voor specifieke domeinen zoals quaternionen voor 3D-graphics. Maar voor de meeste gebruikssituaties is Haskell's `Data.Complex` ruim voldoende.

Onder de motorkap is `Data.Complex` gewoon een datatype dat twee `Float` of `Double` waarden paart, die respectievelijk het reële en imaginaire deel vertegenwoordigen. Het is een eenvoudige en efficiënte manier om met complexe getallen te werken op het Haskell-platform.

## Zie Ook

Bekijk deze bronnen voor meer over complexe getallen in Haskell:

- De officiële Haskell `Data.Complex` documentatie: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Een diepere duik in Haskell's getaltype: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- Voor een toepassing, verken Fast Fourier Transform algoritmen in Haskell: [Haskell FFT library](https://hackage.haskell.org/package/fft)
