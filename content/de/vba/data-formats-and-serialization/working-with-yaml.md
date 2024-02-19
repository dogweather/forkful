---
aliases:
- /de/vba/working-with-yaml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:28.482729-07:00
description: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist eine f\xFC\
  r Menschen lesbare Datenserialisierungssprache, die h\xE4ufig f\xFCr Konfigurationsdateien\u2026"
lastmod: 2024-02-18 23:09:04.713013
model: gpt-4-0125-preview
summary: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist eine f\xFCr Menschen\
  \ lesbare Datenserialisierungssprache, die h\xE4ufig f\xFCr Konfigurationsdateien\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, das für "YAML Ain't Markup Language" steht, ist eine für Menschen lesbare Datenserialisierungssprache, die häufig für Konfigurationsdateien verwendet wird. Programmierer nutzen sie oft wegen ihrer Einfachheit und Lesbarkeit in einer Vielzahl von Programmierumgebungen, einschließlich der Skripterstellung mit Visual Basic for Applications (VBA), um Interoperabilität sowie Datenspeicherung und -austausch zu verbessern.

## Wie geht das:

Mit YAML in VBA zu arbeiten erfordert ein Verständnis dafür, wie man YAML in ein Format umwandelt, das VBA leicht manipulieren kann, normalerweise Wörterbücher oder Sammlungen. Leider unterstützt VBA das Parsen oder Serialisieren von YAML nicht nativ. Sie können jedoch eine Kombination aus JSON-Konvertierungstools und Dictionary-Objekten verwenden, um mit YAML-Daten zu arbeiten, unter Berücksichtigung der engen Beziehung zwischen YAML und JSON.

Zuerst konvertieren Sie Ihre YAML-Daten zu JSON mit einem Online-Konverter oder einem YAML-zu-JSON-Konvertierungstool innerhalb Ihrer Entwicklungsumgebung. Nach der Konvertierung können Sie das folgende Beispiel verwenden, um JSON in VBA zu parsen, und beachten Sie, dass dieser Ansatz es indirekt ermöglicht, mit YAML zu arbeiten:

```vb
' Fügen Sie eine Referenz auf Microsoft Scripting Runtime für Dictionary hinzu
' Fügen Sie eine Referenz auf Microsoft XML, v6.0 zum Parsen von JSON hinzu

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Das ist JSON umgewandelt von YAML
    
    ' Angenommen, Sie haben eine Funktion zum Parsen von JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Platzhalter für die JSON-Parsing-Logik - hier könnte eine externe Bibliothek verwendet werden
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
In diesem Beispiel ist die `JsonParser`-Funktion ein Platzhalter dafür, wo Sie das JSON parsen würden. Verschiedene Bibliotheken sind verfügbar, um beim JSON-Parsing zu helfen, da direkte YAML-Parsing-Bibliotheken für VBA rar sind.

## Vertiefung

Das Fehlen einer direkten YAML-Unterstützung in VBA kann seinem Alter und der Umgebung, für die es erstellt wurde, zugeschrieben werden, die ursprünglich nicht mit modernen Datenserialisierungsformaten im Sinn entwickelt wurde. YAML selbst hat sich als beliebtes Konfigurations- und Serialisierungsformat in den frühen 2000er Jahren herauskristallisiert, parallel zum Aufkommen von Anwendungen, die benutzerfreundlichere Konfigurationsdateien erfordern.

Programmierer nutzen typischerweise externe Tools oder Bibliotheken, um die Lücke zwischen VBA und YAML zu überbrücken. Dies beinhaltet oft die Konvertierung von YAML zu JSON, wie gezeigt, aufgrund der Unterstützung, die durch verschiedene Bibliotheken für JSON verfügbar ist und der Ähnlichkeit zwischen JSON und YAML hinsichtlich Struktur und Zweck.

Obwohl die direkte Arbeit mit YAML in VBA die Flexibilität der Sprache zeigt, ist es erwähnenswert, dass andere Programmierumgebungen (z. B. Python oder JavaScript) nativere und nahtlosere Unterstützung für YAML bieten. Diese Alternativen könnten besser geeignet sein für Projekte, die stark auf YAML für Konfiguration oder Datenserialisierung angewiesen sind. Dennoch bleibt für diejenigen, die sich VBA verpflichtet haben oder es benötigen, die indirekte Methode über JSON-Konvertierung ein praktikabler und nützlicher Ansatz, um YAML-Daten zu verwalten und zu manipulieren.
