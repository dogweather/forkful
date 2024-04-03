---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:39.801247-07:00
description: "JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das sowohl f\xFCr Menschen leicht zu lesen und zu schreiben als auch f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.742162-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das sowohl f\xFCr Menschen leicht zu lesen und zu schreiben als auch f\xFCr Maschinen\
  \ leicht zu parsen und zu generieren ist."
title: Arbeiten mit JSON
weight: 38
---

## Wie:
VBA unterstützt das Parsen oder Generieren von JSON nicht nativ, daher werden wir eine Skriptsprache wie JScript (über das ScriptControl-Objekt) für das Parsen von JSON-Strings und das Erstellen von JSON-Objekten verwenden. Hier sehen Sie, wie Sie einen JSON-String in VBA parsen können:

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
    
    MsgBox "Name: " & parsed.name & ", Alter: " & parsed.age & ", Stadt: " & parsed.city
End Sub
```

Um JSON zu generieren, könnten Sie einen ähnlichen Ansatz verwenden, indem Sie den JSON-String durch Konkatenation aufbauen:

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

## Tiefere Einblicke
Die gezeigten Ansätze nutzen das ScriptControl, um mit JSON umzugehen, und übertragen im Wesentlichen die Arbeit an eine JavaScript-Engine. Dies ist ein kreativer Umweg, aber nicht unbedingt der effizienteste oder modernste Weg, mit JSON in einem VBA-Kontext zu arbeiten. In komplexeren Anwendungen könnte diese Methode umständlich werden und Leistungsüberlastung oder Sicherheitsbedenken einführen, da ScriptControl in einer Umgebung ausgeführt wird, die vollen Zugriff auf den Host-Computer hat.

Andere Programmierumgebungen, wie Python oder JavaScript, bieten eingebaute Unterstützung für JSON und eignen sich daher besser für Anwendungen, die umfangreiche JSON-Manipulationen erfordern. Diese Sprachen bieten umfassende Bibliotheken, die nicht nur das Parsen und Generieren, sondern auch das Abfragen und Formatieren von JSON-Daten erleichtern.

Trotz dieser Einschränkungen in VBA ist es wichtig, zu verstehen, wie man mit JSON arbeitet, in einer Welt, in der der Austausch von webbasierten Daten und Konfigurationsdateien überwiegend im JSON-Format erfolgt. Für VBA-Programmierer eröffnet das Beherrschen dieser Techniken Möglichkeiten, sich mit Web-APIs zu integrieren, Konfigurationsdateien zu interpretieren oder sogar einfache Webanwendungen zu erstellen. Wenn jedoch Projekte an Komplexität zunehmen oder eine hohe Leistung erfordern, sollten Entwickler in Erwägung ziehen, auf programmierfreundlichere Umgebungen umzusteigen.
