---
title:                "Refactoring"
aliases:
- /nl/vba/refactoring/
date:                  2024-02-01T21:59:54.214174-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring in programmeren houdt in dat je de structuur van code wijzigt zonder het gedrag ervan te veranderen, om aspecten zoals leesbaarheid, onderhoudbaarheid of prestaties te verbeteren. Programmeurs refactoren om code efficiënter te maken, gemakkelijker te begrijpen, in de toekomst gemakkelijker te wijzigen en de kans op bugs te verkleinen.

## Hoe:

Beschouw een basisvoorbeeld in Visual Basic for Applications (VBA) waar we een subroutine hebben die de details van een werknemer afdrukt. Aanvankelijk is de code rommelig, moeilijk te onderhouden of uit te breiden.

```vb
Sub PrintEmployeeDetails()
    Dim naam As String
    Dim leeftijd As Integer
    Dim afdeling As String
    naam = "John Doe"
    leeftijd = 30
    afdeling = "IT"
    
    MsgBox "Naam: " & naam & vbCrLf & "Leeftijd: " & leeftijd & vbCrLf & "Afdeling: " & afdeling
End Sub
```

Refactorstap 1: Methode extraheren. Een van de meest voorkomende refactoringtechnieken is een specifiek stuk code te nemen en het in zijn eigen methode te plaatsen. Dit maakt de code meer modulair en gemakkelijker te begrijpen.

```vb
Sub PrintEmployeeDetails()
    Dim naam As String
    Dim leeftijd As Integer
    Dim afdeling As String
    naam = "John Doe"
    leeftijd = 30
    afdeling = "IT"
    
    ToonBericht naam, leeftijd, afdeling
End Sub

Private Sub ToonBericht(naam As String, leeftijd As Integer, afdeling As String)
    MsgBox "Naam: " & naam & vbCrLf & "Leeftijd: " & leeftijd & vbCrLf & "Afdeling: " & afdeling
End Sub
```

Refactorstap 2: Gebruik een structuur. Deze stap houdt in dat je een datastructuur gebruikt om gerelateerde gegevens vast te houden, wat de code duidelijkheid verbetert en het gemakkelijker maakt om gegroepeerde gegevens door te geven.

```vb
Type Werknemer
    naam As String
    leeftijd As Integer
    afdeling As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Werknemer
    emp.naam = "John Doe"
    emp.leeftijd = 30
    emp.afdeling = "IT"
    
    ToonBericht emp
End Sub

Private Sub ToonBericht(emp As Werknemer)
    MsgBox "Naam: " & emp.naam & vbCrLf & "Leeftijd: " & emp.leeftijd & vbCrLf & "Afdeling: " & emp.afdeling
End Sub
```

Deze stappen transformeren rommelige code in modulaire, gestructureerde code, wat de leesbaarheid en onderhoudbaarheid aanzienlijk verbetert.

## Diepgaande duik

Het concept van refactoring is zo oud als programmeren zelf, maar het was Martin Fowler's boek "Refactoring: Improving the Design of Existing Code" dat het in de mainstream bracht, waarbij het belang ervan in het softwareontwikkelingsproces werd benadrukt. In Visual Basic for Applications kan refactoring enigszins uitdagender zijn vanwege het ontbreken aan ingebouwde tools die in modernere geïntegreerde ontwikkelomgevingen (IDE's) worden gevonden en die geautomatiseerde refactoring ondersteunen.

Dit vermindert echter niet het belang ervan. Zelfs in VBA kan het handmatig toepassen van basisrefactoringtechnieken de codebasis aanzienlijk verbeteren, waardoor deze schoner en efficiënter wordt. Hoewel VBA misschien niet dezelfde moderne gemakken heeft, blijven de principes van goed codeontwerp universeel. Ontwikkelaars die uit andere talen komen, kunnen het handmatige proces misschien omslachtig vinden, maar zullen ongetwijfeld de voordelen waarderen van het investeren van tijd in het verbeteren van de codekwaliteit vanaf het begin.

Voor robuustere ontwikkelomgevingen of bij het werken aan bijzonder geavanceerde projecten kan het de moeite waard zijn om alternatieven te verkennen die krachtigere refactoringtools bieden of VBA-projecten naar een .NET-taal te converteren waar Visual Studio uitgebreide refactoringondersteuning biedt. Desalniettemin is het begrijpen en toepassen van refactoringprincipes in VBA een waardevolle vaardigheid die het belang onderstreept van het schrijven van schone, onderhoudbare code, ongeacht de omgeving.
