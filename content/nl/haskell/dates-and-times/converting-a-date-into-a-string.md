---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:47.821006-07:00
description: 'Hoe: In Haskell gebruik je de `formatTime` functie uit de module `Data.Time.Format`
  voor deze taak. Laten we direct duiken in wat code.'
lastmod: '2024-03-13T22:44:50.864002-06:00'
model: gpt-4-0125-preview
summary: In Haskell gebruik je de `formatTime` functie uit de module `Data.Time.Format`
  voor deze taak.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
In Haskell gebruik je de `formatTime` functie uit de module `Data.Time.Format` voor deze taak. Laten we direct duiken in wat code:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- Pak de huidige tijd
    currentTime <- getCurrentTime
    let currentZone = utc
        -- Zet UTC tijd om in een lokaal tijdsobject
        localTime = utcToLocalTime currentZone currentTime
        -- Formatteer de datum als "YYYY-MM-DD"
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

En dit is wat je als uitvoer zou kunnen zien, afhankelijk van de huidige datum:

```
2023-04-01
```

## Diepere Duik
Teruggaand naar de vroege dagen van programmeren, is het omzetten van datums naar strings altijd een kwestie van praktische bruikbaarheid geweest. In Haskell danken we onze datum- en tijdsbehandeling aan de `Data.Time` bibliotheek, die geïnspireerd was door de functionaliteit en verbeteringen ten opzichte van oudere bibliotheken zoals `old-time`.

Er zijn alternatieven voor `formatTime`, zoals het gebruik van `show` om direct een datum naar een string te converteren, maar dit geeft je geen aangepaste formatteringsopties. De `formatTime` functie is rijk, ondersteunend aan een verscheidenheid van formaten die in lijn liggen met de `strftime` functiepatronen van C. Het is flexibel en locatiebewust, met `defaultTimeLocale` of andere locales om datums te formatteren volgens culturele conventies.

Wat betreft de implementatie, de functies in `Data.Time.Format` zijn puur, wat betekent dat ze niet vertrouwen op of bijeffecten veroorzaken. Dit sluit aan bij het ethos van functioneel programmeren in Haskell, dat streeft naar functies die voorspelbaar zijn en waarvan de uitkomsten alleen bepaald worden door hun invoer.

## Zie Ook
Voor uitgebreider werk over datums en tijden in Haskell, bekijk het volgende:

- De documentatie van de `Data.Time` module: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Details over `strftime` formatteerstrings, die `formatTime` imiteert: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- Haskell's benadering van IO en puurheid: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
