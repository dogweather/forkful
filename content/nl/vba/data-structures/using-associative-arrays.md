---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:35.251687-07:00
description: "Associatieve arrays, vaak bekend als woordenboeken in Visual Basic for\
  \ Applications (VBA), stellen programmeurs in staat om collecties van sleutel-\u2026"
lastmod: '2024-03-11T00:14:24.448451-06:00'
model: gpt-4-0125-preview
summary: "Associatieve arrays, vaak bekend als woordenboeken in Visual Basic for Applications\
  \ (VBA), stellen programmeurs in staat om collecties van sleutel-\u2026"
title: Gebruik van associatieve arrays
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, vaak bekend als woordenboeken in Visual Basic for Applications (VBA), stellen programmeurs in staat om collecties van sleutel-waardeparen te creëren. Deze functie is cruciaal voor efficiënte gegevensopslag en -opvraging en biedt een flexibelere en intuïtievere manier om gegevens te beheren dan traditionele array-indices.

## Hoe doe je het:

In VBA biedt het `Dictionary`-object functionaliteit die vergelijkbaar is met associatieve arrays. Eerst moet je een verwijzing naar de Microsoft Scripting Runtime toevoegen om het te gebruiken:

1. Ga in de VBA-editor naar Extra > Referenties...
2. Vink "Microsoft Scripting Runtime" aan en klik op OK.

Hier lees je hoe je een `Dictionary` declareert, vult en items erin benadert:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Items toevoegen
sampleDictionary.Add Key:="Naam", Item:="John Doe"
sampleDictionary.Add Key:="Leeftijd", Item:=29
sampleDictionary.Add Key:="Beroep", Item:="Ingenieur"

' Items benaderen
Debug.Print sampleDictionary.Item("Naam")  ' Uitvoer: John Doe
Debug.Print sampleDictionary.Item("Leeftijd")   ' Uitvoer: 29

' Controleren of een sleutel bestaat
If sampleDictionary.Exists("Beroep") Then
    Debug.Print "Sleutel Beroep Bestaat"
End If

' Items verwijderen
sampleDictionary.Remove("Beroep")

' Door het woordenboek loopen
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Diepgaande Duik

Het `Dictionary`-object maakt intern gebruik van componenten van de Windows Scripting Host. Als zodanig is het een laat-gebonden COM-object, wat een gebruikelijke manier was om de functionaliteit van VBA in het verleden uit te breiden. Het gebruik ervan in VBA kan de mogelijkheid van de taal om complexe datasets te manipuleren aanzienlijk verbeteren zonder een rigide structuur op te leggen, zoals gezien in traditionele arrays of Excel-bereiken.

Een beperking om in gedachten te houden is dat toegang tot het `Dictionary` het instellen van een verwijzing naar de Microsoft Scripting Runtime vereist, wat de distributie van je VBA-projecten kan compliceren. Alternatieven zoals Collections bestaan binnen VBA maar missen enkele van de sleutelfuncties van het `Dictionary`, zoals het gemakkelijk controleren op de aanwezigheid van een sleutel zonder een fout te veroorzaken.

In meer recente programmeercontexten, zoals Python, wordt ingebouwde ondersteuning voor associatieve arrays (ook bekend als woordenboeken in Python) geboden zonder de noodzaak voor het toevoegen van externe verwijzingen. Deze ingebouwde ondersteuning stroomlijnt het proces en biedt vanuit de doos meer geavanceerde functies. Echter, binnen de grenzen van VBA en voor specifieke toepassingen gericht op het automatiseren van taken in de Microsoft Office-suite, blijft het gebruik van het `Dictionary`-object een krachtige en relevante methode voor associatieve array-achtige gegevensstructuren.
