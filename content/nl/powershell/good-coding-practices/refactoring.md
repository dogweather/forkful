---
title:                "Refactoring"
aliases: - /nl/powershell/refactoring.md
date:                  2024-01-28T22:06:24.283311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te veranderen, met als doel de niet-functionele attributen van de software te verbeteren. Programmeurs herschrijven code om deze schoner, efficiënter en eenvoudiger te maken, wat gemakkelijker onderhoud en toekomstige verbeteringen bevordert.

## Hoe:
PowerShell heeft geen ingebouwd specifiek refactoringgereedschap, maar je kunt nog steeds je code opruimen voor leesbaarheid en prestaties. Overweeg een functie die te veel doet en hoe we deze kunnen refactoren voor duidelijkheid:

```PowerShell
function Get-InventoryData {
    # Originele functie die gegevensophaling en -formattering combineert
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $velden = $_ -split ','
        [PSCustomObject]@{
            ItemID = $velden[0]
            Naam   = $velden[1]
            Aantal = $velden[2]
            Prijs  = $velden[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Opnieuw ingedeeld in aparte functies
function Import-InventoryData {
    param($Pad)
    Get-Content -Path $Pad | ForEach-Object {
        $velden = $_ -split ','
        [PSCustomObject]@{
            ItemID = $velden[0]
            Naam   = $velden[1]
            Aantal = $velden[2]
            Prijs  = $velden[3]
        }
    }
}

function Format-InventoryData {
    param($Gegevens)
    $Gegevens | Format-Table -AutoSize
}

# Gebruik
$voorraad = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $voorraad
```

Voorbeelduitvoer:

```
ItemID Naam            Aantal Prijs
------ ----            ------ -----
1001   Widget Type A   50     9.99
1002   Gadget Type B   20     14.99
```

## Diepgaande Duik
Refactoring in programmeren heeft wortels die teruggaan tot de vroegste dagen van softwareontwikkeling, hoewel het pas in de jaren '90 werd geformaliseerd als een praktijk. Martin Fowler's boek "Refactoring: Improving the Design of Existing Code" is een van de fundamentele werken over het onderwerp, en benadrukt het belang van refactoring in het bereiken van schone code.

Hoewel PowerShell niet wordt geleverd met specifieke refactoringtools zoals sommige Integrated Development Environments (IDE's) voor andere talen dat wel doen (denk aan Eclipse of Visual Studio), kun je nog steeds goede refactoringprincipes handmatig toepassen. Het belangrijkste om te onthouden is dat refactoring niet alleen gaat over het veranderen van code omwille van het veranderen, maar om het maken van intentionele, gedrag-behoudende wijzigingen die de structuur en het ontwerp van de code verbeteren.

Alternatieven voor handmatige refactoring in PowerShell omvatten het gebruik van IDE's die de taal ondersteunen, zoals Visual Studio Code met de PowerShell-extensie, die functies zoals codeformatting en basis refactoringmogelijkheden biedt. Voor meer significante refactoring kun je overwegen om Pester-tests te gebruiken om ervoor te zorgen dat wijzigingen de functionaliteit niet veranderen.

Daarnaast kan de implementatie van refactoring meer systemische veranderingen inhouden, zoals modularisatie, waarbij code wordt opgesplitst in herbruikbare modules of functies, wat de naleving van het DRY (Don't Repeat Yourself) principe verbetert. Andere veelvoorkomende refactoringtechnieken zijn het hernoemen voor duidelijkheid, het verwijderen van dubbele code en het verminderen van de complexiteit van conditionele logica.

## Zie Ook
Voor meer diepgaande informatie, hier zijn enkele bronnen:

- Martin Fowler's Refactoring Boek: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Gerefacteerde code testen met Pester: [Pester Testing Framework](https://pester.dev/)
- PowerShell Beste Praktijken: [The PowerShell Best Practices and Style Guide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
