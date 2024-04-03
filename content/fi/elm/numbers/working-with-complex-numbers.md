---
date: 2024-01-26 04:39:26.918818-07:00
description: "Kuinka: Elm ei sis\xE4ll\xE4 valmista tukea kompleksiluvuille, joten\
  \ sinun t\xE4ytyy luoda oma tyyppisi ja funktiosi. T\xE4ss\xE4 on nopea asennus."
lastmod: '2024-03-13T22:44:56.482440-06:00'
model: gpt-4-0125-preview
summary: "Elm ei sis\xE4ll\xE4 valmista tukea kompleksiluvuille, joten sinun t\xE4\
  ytyy luoda oma tyyppisi ja funktiosi."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
Elm ei sisällä valmista tukea kompleksiluvuille, joten sinun täytyy luoda oma tyyppisi ja funktiosi. Tässä on nopea asennus:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Esimerkin käyttö:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

summa = add a b
-- summa on { real = 4.0, imaginary = -2.0 }
```

## Syväsukellus
Historiallisesti kompleksilukuja ei aina ole hyväksytty. Ne muuttuivat pelinrakentajaksi 1500-luvulla ratkaisemaan kuutioyhtälöitä. Vaihtoehtoja muissa kielissä, kuten Python, tarjoavat valmiin tuen kompleksiluvuille operaatioineen suoraan laatikosta. Elm vaatii DIY-lähestymistavan, kuten olet nähnyt. Mutta voit tehdä siitä yhtä hienostuneen kuin tarvitset, rakentaen kertolaskun, jakolaskun ja muita operaatioita, säätäen suorituskykyongelmia.

## Katso myös
- Elmin virallinen dokumentaatio: https://package.elm-lang.org/ omien tyyppien luomiseen ja Elmin perusteiden hallitsemiseen.
- Matematiikan historian harrastajat voisivat tarkistaa "An Imaginary Tale" -kirjan Paul J. Nahinilta kompleksilukujen matkasta ajassa.
- Syvenny matematiikkaan suuntautuviin ohjelmointihaasteisiin Project Eulerissa (https://projecteuler.net) soveltaaksesi kompleksilukujen velhouttasi.
