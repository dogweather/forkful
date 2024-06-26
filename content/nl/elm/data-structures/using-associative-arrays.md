---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:48.884991-07:00
description: 'Hoe te gebruiken: In Elm werk je met Woordenboeken in de `Dict`-module,
  dus laten we duiken in een snel voorbeeld.'
lastmod: '2024-03-13T22:44:50.717471-06:00'
model: gpt-4-0125-preview
summary: In Elm werk je met Woordenboeken in de `Dict`-module, dus laten we duiken
  in een snel voorbeeld.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe te gebruiken:
In Elm werk je met Woordenboeken in de `Dict`-module, dus laten we duiken in een snel voorbeeld:

```Elm
import Dict exposing (Dict)

-- Een woordenboek initialiseren met String-sleutels en Int-waarden
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Een waarde toevoegen of bijwerken
updatedDict = Dict.insert "grape" 10 exampleDict

-- Een waarde ophalen (let op het Maybe-type, aangezien de sleutel mogelijk niet aanwezig is)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Een sleutel-waardepaar verwijderen
finalDict = Dict.remove "banana" updatedDict

-- Een woordenboek terug converteren naar een lijst
dictToList = Dict.toList finalDict
```

Voorbeelduitvoer bij het weergeven van `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Dit demonstreert de basisbewerkingen: creëren, bijwerken, toegang krijgen tot en itereren over een Woordenboek.

## Diepere Duik
Woordenboeken in Elm gebruiken intern een structuur die bekend staat als een AVL-boom - een type zelfbalancerende binaire zoekboom. Deze keuze biedt een evenwicht tussen het garanderen dat operaties zoals invoegen, ophalen en verwijderen een goede prestatie hebben (logaritmische tijdscomplexiteit) en het behouden van eenvoud in de omgang met de gegevens.

Ondanks de sterke punten van Elm's `Dict`, is het geen oplossing die in alle gevallen past. Voor collecties die geordend zijn of sequentieel overlopen moeten worden, kunnen lijsten of arrays toepasselijker zijn. Verder, bij het werken met een vastgestelde set van bekende sleutels, kunnen aangepaste types (Elm's versie van enums) meer typeveiligheid bieden en de intentie in je code duidelijker maken.

In het ecosysteem van Elm biedt `Dict` een betrouwbare manier om collecties van sleutel-waardepartijen te beheren waar de sleutels uniek zijn en de volgorde niet uitmaakt. Hoewel er nieuwere of geavanceerdere structuren kunnen opkomen, blijft de `Dict`-module een fundamentele tool in de gereedschapskist van de Elm-programmeur vanwege zijn eenvoud en efficiëntie in het omgaan met associatieve arrays.
