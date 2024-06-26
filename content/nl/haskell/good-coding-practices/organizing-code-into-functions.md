---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:02.510931-07:00
description: 'Hoe doe je dat: Hier is hoe je functies kunt schrijven en gebruiken
  in Haskell.'
lastmod: '2024-03-13T22:44:50.857842-06:00'
model: gpt-4-0125-preview
summary: Hier is hoe je functies kunt schrijven en gebruiken in Haskell.
title: Code organiseren in functies
weight: 18
---

## Hoe doe je dat:
Hier is hoe je functies kunt schrijven en gebruiken in Haskell:

```Haskell
-- Een eenvoudige functie definiëren om twee getallen op te tellen
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- De functie gebruiken
main = print (addNumbers 3 5)
```

Uitvoer:
```
8
```

Je kunt ook hogere-ordefuncties creëren:

```Haskell
-- Neemt een functie en past deze twee keer toe op iets
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice gebruiken met een anonieme functie
main = print (applyTwice (*2) 5)
```

Uitvoer:
```
20
```

## Diepgaande verkenning
Haskell, een puur functionele taal, behandelt functies als first-class citizens. Historisch gezien is dit geworteld in lambda calculus, een fundamenteel kader in de computatie. In tegenstelling tot imperatieve talen, waar functies een reeks instructies zijn, zijn functies in Haskell uitdrukkingen die relaties tussen gegevens beschrijven.

Er zijn alternatieven voor het schrijven van ruwe functies voor hergebruik. Overweeg het gebruik van typeklassen voor polymorfisme of het benutten van modules om gerelateerde functies te groeperen. Haskell's luie evaluatie heeft ook invloed op de implementatie van functies - functies worden niet geëvalueerd tot hun resultaten nodig zijn, wat prestatieoverwegingen kan beïnvloeden.

## Zie ook
- Officiële Haskell Documentatie: https://www.haskell.org/documentation/
- "Learn You a Haskell for Great Good!" door Miran Lipovača, een beginner vriendelijk boek: http://learnyouahaskell.com/
- "Real World Haskell" door Bryan O'Sullivan, Don Stewart en John Goerzen: http://book.realworldhaskell.org/
