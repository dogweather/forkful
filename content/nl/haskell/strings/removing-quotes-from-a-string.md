---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:10.607518-07:00
description: "Hoe te: In Haskell kunnen we een functie opstellen die alle aanhalingstekens\
  \ uit een gegeven string verwijdert. Het is alsof je de aanhalingstekens de\u2026"
lastmod: '2024-03-13T22:44:50.840304-06:00'
model: gpt-4-0125-preview
summary: In Haskell kunnen we een functie opstellen die alle aanhalingstekens uit
  een gegeven string verwijdert.
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe te:
In Haskell kunnen we een functie opstellen die alle aanhalingstekens uit een gegeven string verwijdert. Het is alsof je de aanhalingstekens de wacht aanzegt, en ervoor zorgt dat ze de hint begrijpen.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell zei, \"Laten we wat functies leren!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Voorbeeld uitvoer:

```
Haskell zei, Laten we wat functies leren!
```

## Diepere Duik
Er was eens, voordat strings in programmeren zo gewoon waren als kattenvideo's op het internet, was het hanteren van tekst een lastige aangelegenheid. Maar naarmate programmeertalen evolueerden, werden strings een cruciaal onderdeel van het coderen. Toch bleven aanhalingstekens een tweesnijdend zwaard—essentieel voor het definiëren van strings, maar een hinder wanneer opgenomen als daadwerkelijke gegevens.

Alternatieven? In plaats van alle aanhalingstekens weg te zwiepen als vliegen, kun je selectief zijn. Misschien wil je alleen de buitenste aanhalingstekens verwijderen (een klassieke trim) of ontsnapte aanhalingstekens binnen een tekenreeks afhandelen.

Wat betreft de implementatie, maakt de `removeQuotes` functie hierboven gebruik van een lambda om elk karakter (`c`) te controleren of het een lastige aanhaling is en ze dienovereenkomstig te filteren. Dit is een rechttoe rechtaan benadering, maar voor grotere teksten of complexere regels, wil je misschien kijken naar parserbibliotheken zoals `Parsec`, die je meer finesse en kracht in tekstverwerking kunnen bieden.

## Zie Ook:
- Voor regex liefhebbers: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Een zachte introductie tot Haskell strings: [Leer je een Haskell voor Groot Goed! - Beginnen](http://learnyouahaskell.com/starting-out#strings)
