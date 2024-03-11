---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:19.495668-07:00
description: "Werken met CSV (Comma Separated Values) bestanden houdt in dat er wordt\
  \ gelezen van of geschreven naar platte tekstbestanden waar de gegevensvelden worden\u2026"
lastmod: '2024-03-11T00:14:24.480738-06:00'
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma Separated Values) bestanden houdt in dat er wordt\
  \ gelezen van of geschreven naar platte tekstbestanden waar de gegevensvelden worden\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV (Comma Separated Values) bestanden houdt in dat er wordt gelezen van of geschreven naar platte tekstbestanden waar de gegevensvelden worden gescheiden door komma's. Programmeurs voeren deze taak vaak uit om gegevensuitwisseling tussen verschillende softwaretoepassingen te vergemakkelijken, gezien de eenvoud en brede acceptatie van het CSV-formaat in verschillende programmeeromgevingen.

## Hoe te:

Visual Basic voor Applications (VBA) vereenvoudigt het werken met CSV-bestanden door ingebouwde functies en methoden die naadloos het lezen van en schrijven naar deze bestanden mogelijk maken. Hieronder volgen voorbeelden die basisbewerkingen met CSV-bestanden illustreren.

### Een CSV-bestand lezen:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath Voor Input Als #1
    
    Do Tot EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Verwerk de dataFields array zoals nodig
        Debug.Print Join(dataFields, ";") 'Voorbeelduitvoer die conversie van komma's naar puntkomma's toont
    Loop
    
    Close #1
End Sub
```

### Naar een CSV-bestand schrijven:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Naam,Leeftijd" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath Voor Output Als #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Voorbeelduitvoer in `output.csv`:
```
ID,Naam,Leeftijd
1,John Doe,30
2,Jane Doe,29
```

## Diepgaand

Historisch gezien zijn CSV-bestanden een eenvoudige methode geweest om tabelgegevens in tekstformaat op te slaan. De eenvoud van zijn structuur, waar elke regel overeenkomt met één gegevensrecord en elk veld binnen een record wordt gescheiden door een komma, is zowel de kracht als de beperking van CSV. Het formaat ondersteunt van nature geen gegevenstypen, wat betekent dat alle gegevens als strings worden opgeslagen, en de last om gegevens naar het juiste type te converteren valt op de programmeur.

In Visual Basic voor Applications wordt omgaan met CSV-bestanden voornamelijk gedaan door basisbestandsbewerkingen, zoals in de eerdere voorbeelden is getoond. Er is geen directe CSV-analyse ondersteuning zoals in modernere talen (bijv. Python's csv-module), die meer controle en gemak biedt bij het omgaan met CSV-gegevens.

Voor meer complexe bewerkingen of bij het werken met grote CSV-bestanden, kunnen programmeurs betere alternatieven vinden buiten puur VBA, zoals het gebruik van externe bibliotheken of het gebruik van andere programmeertalen uitgerust met geavanceerdere CSV-verwerkingsmogelijkheden. Echter, voor eenvoudige taken die verband houden met CSV-bestanden, is VBA's directe aanpak vaak voldoende en gemakkelijk te implementeren, en biedt een snelle oplossing voor Excel-gebaseerde toepassingen of andere automatisering van Microsoft Office-software.
