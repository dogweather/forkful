---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:46.902250-07:00
description: "JSON (JavaScript Object Notation) is een lichtgewicht gegevensuitwisselingsformaat\
  \ dat gemakkelijk te lezen en te schrijven is voor mensen, en eenvoudig\u2026"
lastmod: '2024-02-25T18:49:48.003113-07:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) is een lichtgewicht gegevensuitwisselingsformaat\
  \ dat gemakkelijk te lezen en te schrijven is voor mensen, en eenvoudig\u2026"
title: Werken met JSON
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON (JavaScript Object Notation) is een lichtgewicht gegevensuitwisselingsformaat dat gemakkelijk te lezen en te schrijven is voor mensen, en eenvoudig te parseren en te genereren voor machines. Programmeurs gebruiken JSON om gegevens tussen een server en een webapplicatie te verzenden, of om informatie op een gestructureerde, toegankelijke manier op te slaan binnen een verscheidenheid aan programmeeromgevingen, waaronder Visual Basic for Applications (VBA).

## Hoe te:

VBA ondersteunt van nature geen JSON parsing of generatie, dus we zullen een scripttaal zoals JScript (via het ScriptControl object) gebruiken voor het parsen van JSON strings en het bouwen van JSON objecten. Hier is hoe je een JSON string kunt parsen in VBA:

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
    
    MsgBox "Naam: " & parsed.name & ", Leeftijd: " & parsed.age & ", Stad: " & parsed.city
End Sub
```

Om JSON te genereren, kun je een vergelijkbare benadering gebruiken, de JSON string bouwend door concatenatie:

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

## Diepgaand

De getoonde benaderingen maken gebruik van de ScriptControl om met JSON om te gaan, waarbij het werk in wezen wordt uitbesteed aan een JavaScript engine. Dit is een creatieve oplossing, maar niet noodzakelijkerwijs de meest efficiënte of moderne manier om met JSON in een VBA-context te werken. In meer complexe toepassingen kan deze methode omslachtig worden en prestatieoverhead of veiligheidszorgen introduceren, aangezien ScriptControl wordt uitgevoerd in een omgeving die volledige toegang heeft tot de hostcomputer.

Andere programmeeromgevingen, zoals Python of JavaScript, bieden ingebouwde ondersteuning voor JSON, waardoor ze beter geschikt zijn voor toepassingen die uitgebreide JSON-manipulatie vereisen. Deze talen bieden uitgebreide bibliotheken die niet alleen parsing en generatie faciliteren, maar ook het bevragen en formatteren van JSON-gegevens.

Ondanks deze beperkingen in VBA is begrip van hoe om te gaan met JSON essentieel in een wereld waar webgebaseerde gegevensuitwisseling en configuratiebestanden overwegend JSON-geformatteerd zijn. Voor VBA-programmeurs biedt het beheersen van deze technieken kansen voor integratie met web-API's, het interpreteren van configuratiebestanden of zelfs het bouwen van eenvoudige webtoepassingen. Echter, wanneer projecten groeien in complexiteit of hoge prestaties vereisen, zouden ontwikkelaars kunnen overwegen om meer JSON-vriendelijke programmeeromgevingen te gebruiken.
