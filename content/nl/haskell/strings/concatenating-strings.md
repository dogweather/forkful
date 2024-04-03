---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:14.523765-07:00
description: 'Hoe: Haskell maakt het aaneenschakelen van strings vrij eenvoudig met
  de `(++)` operator.'
lastmod: '2024-03-13T22:44:50.844106-06:00'
model: gpt-4-0125-preview
summary: Haskell maakt het aaneenschakelen van strings vrij eenvoudig met de `(++)`
  operator.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
Haskell maakt het aaneenschakelen van strings vrij eenvoudig met de `(++)` operator:

```Haskell
main :: IO ()
main = doe
  laat hello = "Hallo"
  laat world = "Wereld!"
  
  -- De (++) operator gebruiken
  putStrLn $ hello ++ " " ++ world
  
  -- Voorbeelduitvoer: "Hallo Wereld!"
```

Maar waarom daar stoppen? Je hebt ook `concat` en `intercalate` uit `Data.List` voor wanneer lijsten betrokken raken:

```Haskell
import Data.List (intercalate, concat)

main :: IO ()
main = doe
  laat wordsList = ["Haskell", "is", "cool"]
  
  -- Een lijst van strings aaneenschakelen
  putStrLn $ concat wordsList
  -- Voorbeelduitvoer: "Haskelliscool"

  -- Strings interpoleren met een scheidingsteken
  putStrLn $ intercalate " " wordsList
  -- Voorbeelduitvoer: "Haskell is cool"
```

## Diepere Duik
Vroeger nam de `++` operator van Haskell inspiratie uit soortgelijke bewerkingen in talen zoals ML. Het is een klassieker, maar niet altijd de meest efficiënte, vooral niet voor grote strings of enorme aaneenschakelingstaken. Elk gebruik van `++` creëert een nieuwe lijst, wat betekent dat als je met grote data werkt, je misschien een efficiëntere aanpak nodig hebt.

Alternatieven? Absoluut. Het `Builder` type uit `Data.Text.Lazy.Builder` kan beter geoptimaliseerd zijn voor grote tekstmanipulaties. Het construeert tekst zuiniger door in brokken te werken, waardoor de noodzaak om constant de hele boel te kopiëren vermindert.

Bijvoorbeeld, werken met de `Builder`:

```Haskell
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO as T

main :: IO ()
main = doe
  laat builder1 = fromString "Haskell"
  laat builder2 = fromString " "
  laat builder3 = fromString "is"
  laat builder4 = fromString " "
  laat builder5 = fromString "netjes!"

  laat resultaat = mconcat [builder1, builder2, builder3, builder4, builder5]
  -- mconcat gebruiken om Builders samen te voegen

  T.putStrLn $ toLazyText resultaat
  -- Voorbeelduitvoer: "Haskell is netjes!"
```

Waarom grijp naar `Builder` of `concat`? Ze kunnen grote data aan zonder met de ogen te knipperen, waardoor je tekst kunt combineren zonder te verdrinken in prestatieproblemen.

## Zie Ook
- De Haskell Wiki over [Performance/Strings](https://wiki.haskell.org/Performance/Strings) om dieper in te gaan op prestatieoverwegingen.
- De `Data.Text` [pakketdocumentatie](https://hackage.haskell.org/package/text) voor het werken met Unicode-tekst in Haskell.
- De [Haskell Language website](https://www.haskell.org/) om up-to-date te blijven met alles wat met Haskell te maken heeft.
