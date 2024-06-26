---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:29.682090-07:00
description: "Hoe te: Haskell gebruikt bibliotheken zoals `time` om met datums om\
  \ te gaan. Hier is hoe je dagen of maanden aan een datum toevoegt, of ze aftrekt\
  \ om een\u2026"
lastmod: '2024-03-13T22:44:50.865926-06:00'
model: gpt-4-0125-preview
summary: Haskell gebruikt bibliotheken zoals `time` om met datums om te gaan.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
Haskell gebruikt bibliotheken zoals `time` om met datums om te gaan. Hier is hoe je dagen of maanden aan een datum toevoegt, of ze aftrekt om een verleden datum te vinden.

```Haskell
import Data.Time

-- Dagen aan de huidige datum toevoegen
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  today <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localToday = utcToLocalTime timezone today
  return $ addDays n (localDay localToday)

-- Gebruik: addDaysToCurrent 10 om 10 dagen aan de huidige datum toe te voegen

-- Een toekomstige of verleden datum berekenen door dagen toe te voegen of af te trekken
calculateDate :: Day -> Integer -> Day
calculateDate start n = addDays n start

-- Voorbeeldgebruik:
-- let futureDate = calculateDate (fromGregorian 2023 1 1) 90

-- Om maanden en jaren te behandelen, gebruiken we `addGregorianMonthsClip` en `addGregorianYearsClip`
calculateDateMonths :: Day -> Integer -> Day
calculateDateMonths start n = addGregorianMonthsClip n start

-- Gebruik:
-- let futureMonth = calculateDateMonths (fromGregorian 2023 1 1) 2

-- Een datum uitvoeren in het formaat JJJJ-MM-DD
printFormattedDate :: Day -> IO ()
printFormattedDate date = putStrLn $ formatTime defaultTimeLocale "%F" date

-- Gebruik:
-- printFormattedDate futureDate
```

## Diepgaande duik
In Haskell grijpen we vaak naar de `time` bibliotheek voor datum berekeningen. Deze bibliotheek biedt typen en functies voor DateTime rekenkunde, ontleding en opmaak. Historisch gezien zouden mensen handmatig datums aanpassen, maar bibliotheken zoals `time` behandelen de eigenaardigheden van kalenders (zoals schrikkeljaren).

Alternatieven voor `time` zijn `Data.Time.Calendar.OrdinalDate` en `Data.Time.Clock.POSIX` voor verschillende behoeften, zoals werken met weeknummers of tijdstempels.

Wat betreft de implementatie is het berekenen van datums verrassend complex. Zelfs met `time` zorgen functies zoals `addGregorianMonthsClip` ervoor dat de resulterende datum geldig is. Bijvoorbeeld, het toevoegen van één maand aan 31 januari zal "clippen" naar de laatste dag van februari (ofwel de 28ste of 29ste), en niet naar 3 maart.

## Zie ook
- Haskell `time` bibliotheek: http://hackage.haskell.org/package/time
- Datum- en Tijdgids van The Haskell School: https://school.haskellforall.com/#date-and-time
- Uitleg over ZonedTime en UTC: https://www.47deg.com/blog/dealing-with-time-in-haskell/
