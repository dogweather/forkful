---
aliases:
- /nl/vba/removing-quotes-from-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:13.000506-07:00
description: "Het verwijderen van aanhalingstekens uit een string in VBA houdt in\
  \ dat enkele (`'`) of dubbele (`\"`) aanhalingstekens, die een string kunnen omsluiten\
  \ of\u2026"
lastmod: 2024-02-18 23:09:01.652252
model: gpt-4-0125-preview
summary: "Het verwijderen van aanhalingstekens uit een string in VBA houdt in dat\
  \ enkele (`'`) of dubbele (`\"`) aanhalingstekens, die een string kunnen omsluiten\
  \ of\u2026"
title: Quotes uit een string verwijderen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van aanhalingstekens uit een string in VBA houdt in dat enkele (`'`) of dubbele (`"`) aanhalingstekens, die een string kunnen omsluiten of er binnen ingebed zijn, worden weggehaald. Deze bewerking is essentieel voor het schoonmaken van gegevens, om te zorgen dat strings correct geformatteerd zijn voor database query's, JSON parsing, of simpelweg voor esthetische of consistentieredenen binnen de interface van een applicatie.

## Hoe:

In VBA zijn er meerdere benaderingen om aanhalingstekens uit een string te verwijderen. Hier is een eenvoudig voorbeeld dat de `Replace` functie gebruikt, die zoekt naar een specifieke subreeks (in dit geval, een aanhalingsteken) binnen een reeks en deze vervangt door een andere subreeks (een lege string als het verwijderen betreft).

```basic
Sub VerwijderAanhalingstekensVoorbeeld()
    Dim origineleString Als String
    origineleString = "'Dit' is een ""test"" reeks."
    
    ' Enkele aanhalingstekens verwijderen
    origineleString = Replace(origineleString, "'", "")
    
    ' Dubbele aanhalingstekens verwijderen
    origineleString = Replace(origineleString, Chr(34), "")
    
    Debug.Print origineleString 'Uitvoer: Dit is een test reeks.
End Sub
```

Let op dat voor dubbele aanhalingstekens we `Chr(34)` gebruiken omdat een dubbel aanhalingsteken het ASCII karakter 34 is. Dit is noodzakelijk aangezien dubbele aanhalingstekens ook gebruikt worden voor het aangeven van string literals in VBA.

Voor meer genuanceerde scenario's waarin aanhalingstekens deel kunnen uitmaken van de nodige opmaak (bijv. binnen een aangehaald woord), kan meer verfijnde logica, misschien met behulp van Regex of door karakter voor karakter te parseren, vereist zijn.

## Diepere Duik

VBA, als een hoeksteen voor het automatiseren van taken binnen de Microsoft Office suite, biedt een rijke set van functies voor string manipulatie, met `Replace` als een van de meest gebruikt. Deze functie, echter, raakt slechts het oppervlak van wat er bereikt kan worden met VBA in termen van string manipulatie.

Historisch gezien heeft VBA van zijn voorgangers een nadruk op eenvoud voor office automatiseringstaken overgenomen, vandaar de rechttoe rechtaan implementatie van functies zoals `Replace`. Echter, voor moderne programmeeropgaven, vooral die welke complexe string manipulaties of saneringen betreffen, kan VBA zijn beperkingen tonen.

In dergelijke gevallen kunnen programmeurs resorteren tot het combineren van VBA met reguliere expressies (via het `VBScript_RegExp_55.RegExp` object) voor meer flexibiliteit en kracht in het parsen en manipuleren van strings. Deze aanpak introduceert echter extra complexiteit en vereist een degelijk begrip van regex patronen, wat mogelijk niet geschikt is voor alle gebruikers.

Ondanks zijn beperkingen dekt VBA's `Replace` functie efficiënt veel voorkomende scenario's af met betrekking tot het verwijderen van aanhalingstekens uit strings. Het biedt een snelle en gemakkelijke oplossing voor de meeste behoeften aan string manipulatie zonder in het meer complexe regex gebied te duiken. Voor degenen die de limieten bereiken van wat `Replace` en andere basis stringfuncties kunnen doen, kan het verkennen van regex binnen VBA of het overwegen van een robuustere taal die is afgestemd op complexe stringbewerkingen de volgende beste stappen zijn.
