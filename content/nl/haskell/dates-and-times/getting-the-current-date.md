---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:48.769084-07:00
description: 'Hoe doe je het: In Haskell krijg je de huidige datum met behulp van
  de `Data.Time` bibliotheek. Eerst importeer je wat je nodig hebt.'
lastmod: '2024-03-13T22:44:50.863037-06:00'
model: gpt-4-0125-preview
summary: In Haskell krijg je de huidige datum met behulp van de `Data.Time` bibliotheek.
title: Het huidige datum ophalen
weight: 29
---

## Hoe doe je het:
In Haskell krijg je de huidige datum met behulp van de `Data.Time` bibliotheek. Eerst importeer je wat je nodig hebt:

```haskell
import Data.Time
```

Pak nu de datum van vandaag:

```haskell
main :: IO ()
main = do
    today <- getCurrentTime
    putStrLn $ "De datum van vandaag is: " ++ show (utctDay today)
```

Een voorbeelduitvoer ziet er zo uit:

```
De datum van vandaag is: 2023-03-23
```

## Uitgebreid
Haskell houdt zich al sinds zijn vroege dagen bezig met datum-tijd, de `Data.Time` bibliotheek is geëvolueerd uit oudere tijdsbibliotheken. Het heeft alles wat je nodig hebt direct beschikbaar, maar kan een beetje intimiderend zijn. Er bestaan alternatieven, zoals `time-recurrence` voor gepatroonde datum berekeningen, of `old-time`, vroeger de go-to van Haskell voor datum-tijd operaties.

`Data.Time` werkt veel met `UTCTime`, de universele tijdstandaard. Maar je kunt ook met tijdzones werken met behulp van `ZonedTime` uit dezelfde bibliotheek. Het werkt door een `LocalTime` (datum en tijd zonder zone) en een `TimeZone` die de afwijking van `UTC` aangeeft, te combineren.

## Zie ook
- "Learn You a Haskell" voor tijdgerelateerde operaties: [http://learnyouahaskell.com](http://learnyouahaskell.com/)
- Het omgaan met tijdzones in Haskell: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
