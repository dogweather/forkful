---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:28.360035-07:00
description: "Hoe te: Haskell, stilletjes bekend om zijn puurheid, vereist dat je\
  \ de juiste bibliotheken gebruikt om over datums te praten. Laten we `Data.Time`\u2026"
lastmod: '2024-03-13T22:44:50.864947-06:00'
model: gpt-4-0125-preview
summary: Haskell, stilletjes bekend om zijn puurheid, vereist dat je de juiste bibliotheken
  gebruikt om over datums te praten.
title: Twee datums vergelijken
weight: 27
---

## Hoe te:
Haskell, stilletjes bekend om zijn puurheid, vereist dat je de juiste bibliotheken gebruikt om over datums te praten. Laten we `Data.Time` gebruiken.

```haskell
import Data.Time

-- Definieer twee datums
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- Vergelijk de datums
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = doe
    print $ date1 `compareDates` date2 -- Uitvoer zal LT zijn
    print $ date2 `compareDates` date1 -- Uitvoer zal GT zijn
    print $ date1 `compareDates` date1 -- Uitvoer zal EQ zijn
```

Rechttoe rechtaan, toch? `LT` voor minder dan, `GT` voor groter dan, en `EQ` voor gelijk.

## Diepgang
Vroeger was de tijdafhandeling in Haskell niet zo soepel. We hebben onze huidige gemakken te danken aan de ontwikkeling van de `Data.Time` bibliotheek door de jaren heen. Het geeft ons `UTCTime`, een blije ondubbelzinnige aanduiding van tijd.

Alternatieven? Zeker. Je vindt misschien `Data.Time.Calendar` en `Data.Time.Clock` nuttig voor specifieke scenario’s. Er is ook de oude `time` bibliotheek voor degenen die nostalgisch zijn of vastzitten met verouderde code.

Nu, de details: Het vergelijken van datums in Haskell hangt af van `UTCTime` die een dag (`Day`) en een tijd (`DiffTime` of `NominalDiffTime`) combineert. Het is de `compare` functie die het zware werk doet, een net lid van de `Ord` klasse, waardoor we `>, <, ==` en meer kunnen gebruiken. Onthoud gewoon dat Haskell houdt van typeveiligheid. Zorg ervoor dat je altijd appels met appels vergelijkt, of in ons geval, `UTCTime` met `UTCTime`.

## Zie Ook
Duik dieper of zoek hulp met deze:
- [`Data.Time` pakket op Hackage](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [Leer Je een Haskell voor Groot Plezier! - Voor een zachte introductie](http://learnyouahaskell.com/)
- [Stack Overflow voor het oplossen van problemen in de echte wereld](https://stackoverflow.com/questions/tagged/haskell+time)
