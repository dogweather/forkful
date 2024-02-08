---
title:                "Refactoring"
aliases:
- nl/lua/refactoring.md
date:                  2024-01-28T22:05:57.583063-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoren is de kunst van het aanpassen van bestaande code om de structuur, leesbaarheid en efficiëntie ervan te verbeteren, zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om hun code beter onderhoudbaar te maken, complexiteit te verminderen en vaak als een voorbereidende stap voordat ze nieuwe functies toevoegen of bugs repareren.

## Hoe:
Laten we een eenvoudige Lua-functie nemen en refactoren. We beginnen met een functie die de som van nummers in een lijst berekent, maar zonder veel aandacht voor efficiëntie of duidelijkheid is geschreven:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Uitvoer: 10
```

Refactor naar een efficiëntere en leesbaardere versie:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Geeft ook uit: 10
```

De gerefactoreerde versie verwijdert de overbodige binnenlus, door `ipairs` te gebruiken om netjes door de lijst te itereren.

## Diepere Duik
Historisch gezien komt refactoren uit de Smalltalk-programmeergemeenschap in de late jaren 80 en werd het gepopulariseerd door het boek 'Refactoring: Improving the Design of Existing Code' van Martin Fowler. In Lua omvat refactoren vaak het vereenvoudigen van complexe conditionals, het opsplitsen van grote functies in kleinere, en het optimaliseren van tabelgebruik om de prestaties te verbeteren.

Refactoren in Lua heeft zijn valkuilen; de dynamische aard van Lua en flexibele typen kunnen bepaalde refactors, zoals het hernoemen van variabelen of het veranderen van functiesignaturen, riskanter maken als ze niet voorzichtig worden uitgevoerd. Tools voor statische codeanalyse (zoals `luacheck`) kunnen dergelijke risico's verminderen. Alternatieven omvatten testgedreven ontwikkeling (TDD), waar code continu wordt gerefactoreerd als een integraal onderdeel van het ontwikkelingsproces, in tegenstelling tot een aparte refactorfase.

## Zie Ook
- "Programming in Lua" van Roberto Ierusalimschy voor beste praktijken en voorbeelden.
- "Refactoring: Improving the Design of Existing Code" van Martin Fowler voor principes die van toepassing zijn over talen heen.
- LuaRocks directory (https://luarocks.org/) voor hulpmiddelen en modules gericht op het onderhouden en refactoren van Lua-code.
