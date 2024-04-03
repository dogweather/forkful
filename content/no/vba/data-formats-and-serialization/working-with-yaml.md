---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:27.050005-07:00
description: "YAML, som st\xE5r for \"YAML Ain't Markup Language\", er et menneskelesbart\
  \ data serialiseringsspr\xE5k som ofte brukes for konfigurasjonsfiler. Programmerere\u2026"
lastmod: '2024-03-13T22:44:40.643963-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for \"YAML Ain't Markup Language\", er et menneskelesbart\
  \ data serialiseringsspr\xE5k som ofte brukes for konfigurasjonsfiler."
title: Arbeider med YAML
weight: 41
---

## Hva og hvorfor?

YAML, som står for "YAML Ain't Markup Language", er et menneskelesbart data serialiseringsspråk som ofte brukes for konfigurasjonsfiler. Programmerere bruker det ofte på grunn av dets enkelhet og lesbarhet på tvers av et mylder av programmeringsmiljøer, inkludert i scriptingverdenen av Visual Basic for Applications (VBA) for å forbedre interoperabilitet, samt lagring og utveksling av data.

## Hvordan:

Å jobbe med YAML i VBA krever forståelse for hvordan man analyserer og konverterer YAML til et format som VBA kan manipulere enkelt, vanligvis ordbøker eller samlinger. Dessverre støtter ikke VBA YAML-analyse eller serialisering nativt. Imidlertid kan du bruke en kombinasjon av JSON-konverteringsverktøy og ordboksobjekter for å jobbe med YAML-data, med tanke på YAMLs nære forhold til JSON.

Først, konverter dine YAML-data til JSON ved hjelp av en nettbasert konverterer eller et YAML-til-JSON konverteringsverktøy i ditt utviklingsmiljø. Når konvertert, kan du bruke følgende eksempel for å analysere JSON i VBA, og merk at denne tilnærmingen indirekte lar deg jobbe med YAML:

```vb
' Legg til referanse til Microsoft Scripting Runtime for Dictionary
' Legg til referanse til Microsoft XML, v6.0 for JSON-analyse

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Dette er JSON konvertert fra YAML
    
    ' Anta at du har en JSON-parser funksjon
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Navn: " & parsedData("name")
    Debug.Print "Alder: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Plassholder for JSON-analyse logikk - du kan bruke et eksternt bibliotek her
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
I dette eksempelet er `JsonParser` funksjonen en plassholder for hvor du ville analysert JSON. Diverse biblioteker er tilgjengelige for å hjelpe med JSON-analyse, ettersom direkte YAML-analysebiblioteker for VBA er sjeldne.

## Dypdykk

Fraværet av direkte YAML-håndtering i VBA kan tilskrives dets alder og det miljøet det ble bygget for, som ikke opprinnelig var designet med moderne data serialiseringsformater i tankene. YAML selv dukket opp som et populært konfigurasjons- og serialiseringsformat i begynnelsen av 2000-tallet, i samtid med fremveksten av applikasjoner som krevde mer menneskevennlige konfigurasjonsfiler.

Programmerere benytter vanligvis eksterne verktøy eller biblioteker for å tette gapet mellom VBA og YAML. Dette innebærer ofte å konvertere YAML til JSON, som vist, på grunn av JSON-støtten som er tilgjengelig gjennom diverse biblioteker og likheten mellom JSON og YAML når det gjelder struktur og hensikt.

Selv om å jobbe direkte med YAML i VBA viser språkets fleksibilitet, er det verdt å merke seg at andre programmeringsmiljøer (for eksempel Python eller JavaScript) tilbyr mer innfødt og sømløs støtte for YAML. Disse alternativene kan være bedre egnet for prosjekter som i stor grad er avhengige av YAML for konfigurasjon eller dataserialisering. Likevel, for de som er forpliktet til eller krever VBA, forblir den indirekte metoden gjennom JSON-konvertering en levedyktig og nyttig tilnærming for å håndtere og manipulere YAML-data.
