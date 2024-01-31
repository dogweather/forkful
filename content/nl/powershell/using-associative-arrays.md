---
title:                "Gebruik van associatieve arrays"
date:                  2024-01-30T19:12:34.690871-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van associatieve arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, ook bekend als hash-tabellen of woordenboeken in PowerShell, stellen je in staat om gegevens op te slaan in sleutel-waardeparen, wat de gegevensopvraging eenvoudig en efficiënt maakt. Programmeurs gebruiken ze om gerelateerde gegevens samen op te slaan op een manier die gemakkelijk toegankelijk is op sleutel.

## Hoe:

Het creëren en gebruiken van associatieve arrays in PowerShell is vrij eenvoudig. Zo doe je de magie:

**Een associatieve array creëren:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Ingenieur"
```

Dit codefragment creëert een associatieve array met drie sleutel-waardeparen.

**Waarden opvragen:**

Om een waarde te krijgen, verwijs je naar zijn sleutel:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Voorbeelduitvoer:**

```
Alex
```

**Gegevens toevoegen of wijzigen:**

Gebruik gewoon de sleutel om een nieuw paar toe te voegen of een bestaand paar te wijzigen:

```PowerShell
$myAssociativeArray["location"] = "New York" # Voegt een nieuw sleutel-waardepaar toe
$myAssociativeArray["job"] = "Senior Ingenieur" # Wijzigt een bestaand paar
```

**Itereren over een associatieve array:**

Loop zo door sleutels en waarden:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Voorbeelduitvoer:**

```
name : Alex
age : 25
job : Senior Ingenieur
location : New York
```

## Verdieping

Het concept van associatieve arrays is gemeenschappelijk in veel programmeertalen, meestal een woordenboek, kaart, of hash-tabel genoemd, afhankelijk van de taal. In PowerShell worden associatieve arrays geïmplementeerd als hash-tabellen, die zeer efficiënt zijn voor het opzoeken van sleutels, het opslaan van gegevens en het onderhouden van een collectie van unieke sleutels.

Historisch gezien bieden associatieve arrays een middel om collecties van objecten te beheren waar elk item snel kan worden opgehaald zonder door de hele collectie heen te itereren, met behulp van zijn sleutel. De efficiëntie van gegevensopvraging en -wijziging in associatieve arrays maakt ze een voorkeurskeuze voor verschillende taken. Ze hebben echter beperkingen, zoals het behouden van volgorde, waarvoor geordende woordenboeken of aangepaste objecten een beter alternatief zouden kunnen zijn.

Ondanks hun beperkingen zijn associatieve arrays/hash-tabellen in PowerShell ongelooflijk flexibel en een krachtig hulpmiddel voor scripting. Ze bieden dynamische gegevensopslag en zijn vooral nuttig in configuraties, gegevensmanipulatie en overal waar een gestructureerd gegevensformaat nodig is zonder de overhead van een formele klasse-definitie. Onthoud wel, terwijl associatieve arrays perfect zijn voor op sleutels gebaseerde opvraging, als je taak complexe gegevensstructuren omvat of een specifieke volgorde moet aanhouden, wil je misschien andere gegevenstypen of aangepaste objecten binnen PowerShell verkennen.
