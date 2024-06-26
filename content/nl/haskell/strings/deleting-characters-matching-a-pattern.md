---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:45.201706-07:00
description: "Hoe te: Haskell's rijke set aan bibliotheken, zoals 'Data.List', biedt\
  \ verschillende hulpmiddelen om lijsten te manipuleren, waarvan strings in essentie\u2026"
lastmod: '2024-04-05T21:53:50.860019-06:00'
model: gpt-4-0125-preview
summary: Haskell's rijke set aan bibliotheken, zoals 'Data.List', biedt verschillende
  hulpmiddelen om lijsten te manipuleren, waarvan strings in essentie een speciaal
  geval zijn.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe te:
```haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- Eenvoudige functie om een patroon uit een string te verwijderen
removePattern :: Eq a => [a] -> [a] -> [a]
removePattern [] _ = []
removePattern string@(x:xs) patroon
  | patroon `isInfixOf` string = removePattern (drop (length patroon) string) patroon
  | anders = x : removePattern xs patroon

-- Gebruik vooraf gedefinieerde functies om spaties uit een string te knippen
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let text = "Haskell is super cool, super cool inderdaad."
  let cleanedText = removePattern text "super "
  putStrLn cleanedText  -- "Haskell is cool, cool inderdaad."
  putStrLn $ trim "   Getrimde witruimte   " -- "Getrimde witruimte"
```

## Diepgaande Duik
Haskell's rijke set aan bibliotheken, zoals 'Data.List', biedt verschillende hulpmiddelen om lijsten te manipuleren, waarvan strings in essentie een speciaal geval zijn. Historisch gezien is het patroonherkenningconcept van Haskell geleend van oudere functionele talen zoals ML.

Er zijn verschillende manieren van patroonherkenning in Haskell. Onze eenvoudige `removePattern` maakt gebruik van `isInfixOf` om het patroon te controleren. Er zijn ook regex-bibliotheken voor complexe patronen, maar deze voegen afhankelijkheden toe en maken soms dingen overbodig ingewikkeld.

Wat betreft afhankelijkheden, voor het trimmen van witruimtes, zou je een bibliotheek van derden kunnen importeren, maar onze 'trim'-functie doet het werk op een native manier.

Ten slotte, wat betreft prestaties, wees altijd voorzichtig met recursieve functies in Haskell; ze kunnen inefficiënt zijn als ze niet goed geoptimaliseerd zijn door de compiler. Thunks kunnen zich opstapelen, wat kan leiden tot geheugenlekken. Voor betere prestaties zou je Haskell's `Text` module kunnen verkennen voor de manipulatie van grote of talrijke strings.

## Zie Ook
- Echt Wereld Haskell: http://book.realworldhaskell.org/
- Haskell `Data.List` documentatie: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html
- Haskell Wiki over Prestaties: https://wiki.haskell.org/Performance
