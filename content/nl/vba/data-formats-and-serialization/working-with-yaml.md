---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:20.825237-07:00
description: "YAML, wat staat voor \"YAML Ain't Markup Language\", is een voor mensen\
  \ leesbare gegevensserialisatietaal die vaak wordt gebruikt voor\u2026"
lastmod: '2024-03-13T22:44:50.658207-06:00'
model: gpt-4-0125-preview
summary: "YAML, wat staat voor \"YAML Ain't Markup Language\", is een voor mensen\
  \ leesbare gegevensserialisatietaal die vaak wordt gebruikt voor\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, wat staat voor "YAML Ain't Markup Language", is een voor mensen leesbare gegevensserialisatietaal die vaak wordt gebruikt voor configuratiebestanden. Programmeurs gebruiken het vaak vanwege de eenvoud en leesbaarheid in een verscheidenheid aan programmeeromgevingen, inclusief in het scriptingdomein van Visual Basic for Applications (VBA) om de interoperabiliteit, en gegevensopslag en -uitwisseling te verbeteren.

## Hoe te:

Werken met YAML in VBA vereist begrip van hoe YAML geparseerd en geconverteerd kan worden naar een formaat dat VBA gemakkelijk kan manipuleren, meestal woordenboeken of collecties. Helaas ondersteunt VBA YAML-parsing of serialisatie niet van nature. Echter, je kunt een combinatie van JSON-conversiehulpmiddelen en dictionary-objecten gebruiken om met YAML-gegevens te werken, rekening houdend met de nauwe relatie tussen YAML en JSON.

Converteer eerst je YAML-gegevens naar JSON met behulp van een online converter of een YAML-naar-JSON-conversiehulpmiddel binnen je ontwikkelomgeving. Eenmaal geconverteerd, kun je het volgende voorbeeld gebruiken om JSON in VBA te parsen, erop wijzend dat deze aanpak je indirect toestaat om met YAML te werken:

```vb
' Voeg referentie toe aan Microsoft Scripting Runtime voor Dictionary
' Voeg referentie toe aan Microsoft XML, v6.0 voor JSON parsing

Sub ParseYAMLAlsJSON()
    Dim jsonText Als String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Dit is JSON geconverteerd van YAML
    
    ' Aangenomen dat je een JSON parser functie hebt
    Dim parsedData Als Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Naam: " & parsedData("name")
    Debug.Print "Leeftijd: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText Als String) Als Dictionary
    ' Placeholder voor JSON parsing logica - je zou hier een externe bibliotheek kunnen gebruiken
    Set JsonParser = Nieuw Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
In dit voorbeeld is de `JsonParser` functie een stand-in voor waar je de JSON zou parsen. Verschillende bibliotheken zijn beschikbaar om te helpen met JSON-parsing, aangezien directe YAML-parsingbibliotheken voor VBA schaars zijn.

## Diep Duiken

De afwezigheid van directe YAML-handeling in VBA kan worden toegeschreven aan de leeftijd ervan en de omgeving waarvoor het is gebouwd, die oorspronkelijk niet was ontworpen met moderne gegevensserialisatieformaten in gedachten. YAML zelf kwam naar voren als een populair configuratie- en serialisatieformaat in de vroege jaren 2000, samenvallend met de opkomst van applicaties die meer mensvriendelijke configuratiebestanden vereisen.

Programmeurs maken doorgaans gebruik van externe hulpmiddelen of bibliotheken om de kloof tussen VBA en YAML te overbruggen. Dit houdt vaak in dat YAML naar JSON wordt geconverteerd, zoals getoond, vanwege de beschikbare JSON-ondersteuning via verschillende bibliotheken en de overeenkomst tussen JSON en YAML qua structuur en doel.

Hoewel het direct werken met YAML in VBA de flexibiliteit van de taal laat zien, is het vermeldenswaard dat andere programmeeromgevingen (bijv. Python of JavaScript) meer native en naadloze ondersteuning bieden voor YAML. Deze alternatieven kunnen beter geschikt zijn voor projecten die sterk afhankelijk zijn van YAML voor configuratie of gegevensserialisatie. Desalniettemin blijft voor degenen die zich inzetten voor of VBA nodig hebben de indirecte methode via JSON-conversie een haalbare en nuttige aanpak om YAML-gegevens te beheren en te manipuleren.
