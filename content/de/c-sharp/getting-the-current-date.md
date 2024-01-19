---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Arbeiten mit aktuellem Datum in C#

## Was & Warum?
Das aktuelle Datum zu ermitteln ist eine häufig verwendete Funktion in der Programmierung, um die momentane Zeit abzugreifen. Die Anwendungen sind vielfältig, vom Timing von Operationen bis hin zur Erstellung von Zeitstempeln für Log-Einträge.

## Wie geht das:
Die C#-Klasse "DateTime" enthält Funktionen zum Ermitteln des aktuellen Datums und der aktuellen Zeit. Hier sind einige Beispielverwendungen:

```C#
// Aktuelles Datum
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate); 

// Aktuelles Datum im Jahr-Monat-Tag Format
string formattedDate = DateTime.Now.ToString("yyyy-MM-dd");
Console.WriteLine(formattedDate); 
```
Wenn das Programm ausgeführt wird, würde es das aktuelle Datum und die aktuelle Uhrzeit, sowie das formatierte Datum ausgeben.

## Deep Dive
Die DateTime-Klasse ist seit dem .NET Framework 1.1 Bestandteil von C#. Sie enthält eine Reihe von Methoden und Eigenschaften, um mit Daten und Zeiten zu arbeiten. Als Alternative können Sie auch die "DateTimeOffset"-Klasse verwenden, die zusätzlich zu Datum und Uhrzeit auch die Zeitzonendifferenz speichert.

Effizienz ist ebenfalls ein wichtiger Aspekt bei der Arbeit mit Daten und Zeiten. Bei der Verarbeitung von großen Datenmengen können Konvertierungen von DateTime zu String und umgekehrt zu Leistungseinbußen führen. Deshalb sollten Sie solche Operationen minimieren.

## Siehe auch:
- Microsoft Dokumentation zu DateTime: https://docs.microsoft.com/de-de/dotnet/api/system.datetime
- Microsoft Artikel über die Verwendung von Datum und Uhrzeit: https://docs.microsoft.com/de-de/dotnet/standard/datetime
- Grundlagen der C#-Programmierung auf Deutsch: https://www.codeproject.com/Articles/2833/Csharp-Programmierung