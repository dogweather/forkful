---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:44.446058-07:00
description: "JSON (JavaScript Object Notation) er et lettvekts datautvekslingsformat\
  \ som er lett for mennesker \xE5 lese og skrive, og for maskiner \xE5 analysere\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.645212-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) er et lettvekts datautvekslingsformat\
  \ som er lett for mennesker \xE5 lese og skrive, og for maskiner \xE5 analysere\
  \ og\u2026"
title: "\xC5 Arbeide med JSON"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

JSON (JavaScript Object Notation) er et lettvekts datautvekslingsformat som er lett for mennesker å lese og skrive, og for maskiner å analysere og generere. Programmerere bruker JSON til å overføre data mellom en server og en webapplikasjon eller til å lagre informasjon på en strukturert, tilgjengelig måte innenfor en rekke programmeringsmiljøer, inkludert Visual Basic for Applications (VBA).

## Hvordan:

VBA støtter ikke JSON-analyse eller generering som standard, så vi vil bruke et skriptspråk som JScript (via ScriptControl-objektet) for å analysere JSON-strenger og bygge JSON-objekter. Slik kan du analysere en JSON-streng i VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Navn: " & parsed.name & ", Alder: " & parsed.age & ", By: " & parsed.city
End Sub
```

For å generere JSON, kunne du bruke en lignende tilnærming, som bygger JSON-strengen gjennom sammenføyning:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Dypdykk

Tilnærmingene som vises utnytter ScriptControl for å håndtere JSON, som i bunn og grunn setter ut arbeidet til en JavaScript-motor. Dette er en kreativ omgåelse, men ikke nødvendigvis den mest effektive eller moderne måten å arbeide med JSON i en VBA-kontekst. I mer komplekse applikasjoner kan denne metoden bli tungvint og introdusere ytelsesoverhead eller sikkerhetsproblemer, siden ScriptControl utfører i et miljø som har full tilgang til vertsmaskinen.

Andre programmeringsmiljøer, som Python eller JavaScript, tilbyr innebygd støtte for JSON, noe som gjør dem mer egnet for applikasjoner som krever omfattende JSON-manipulasjon. Disse språkene gir omfattende biblioteker som letter ikke bare analyse og generering, men også spørring og formatering av JSON-data.

Til tross for disse begrensningene i VBA, er det å forstå hvordan man arbeider med JSON avgjørende i en verden hvor webbasert datautveksling og konfigurasjonsfiler for det meste er formatert som JSON. For VBA-programmerere, å mestre disse teknikkene åpner opp muligheter for integrering med web-APIer, tolkning av konfigurasjonsfiler eller til og med bygging av enkle webapplikasjoner. Men, når prosjekter vokser i kompleksitet eller krever høy ytelse, kan utviklere vurdere å utnytte programmeringsmiljøer som er mer vennlige for JSON.
