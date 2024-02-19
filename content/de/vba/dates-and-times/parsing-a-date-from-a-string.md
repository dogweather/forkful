---
aliases:
- /de/vba/parsing-a-date-from-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:36.322861-07:00
description: "Das Parsen eines Datums aus einem String in Visual Basic for Applications\
  \ (VBA) bedeutet, einen Text, der ein Datum repr\xE4sentiert, in einen Datumsdatentyp\u2026"
lastmod: 2024-02-18 23:09:04.700335
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String in Visual Basic for Applications\
  \ (VBA) bedeutet, einen Text, der ein Datum repr\xE4sentiert, in einen Datumsdatentyp\u2026"
title: Einen Datum aus einem String auslesen
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String in Visual Basic for Applications (VBA) bedeutet, einen Text, der ein Datum repräsentiert, in einen Datumsdatentyp umzuwandeln. Programmierer tun dies, um Daten in ihren Anwendungen effektiver manipulieren zu können, beispielsweise für Vergleiche, Berechnungen oder Formatierungszwecke.

## Wie geht das:

VBA bietet eine unkomplizierte Möglichkeit, einen String mit der Funktion `CDate` oder der Funktion `DateValue` in ein Datum zu parsen. Es ist jedoch entscheidend, dass der String in einem erkennbaren Datumsformat vorliegt.

Hier ist ein einfaches Beispiel mit `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Geparstes Datum: "; parsedDate
End Sub
```

Wenn Sie diesen Code ausführen, wäre die Ausgabe im Direktfenster (zugänglich über `Strg+G` im VBA-Editor):

```
Geparstes Datum: 1.4.2023 
```

Alternativ können Sie die Funktion `DateValue` verwenden, die spezifischer für Daten ist (ohne Berücksichtigung der Uhrzeit):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "1. April 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Geparstes Datum mit DateValue: "; parsedDate
End Sub
```

Die Ausgabe dafür würde ähnlich im Direktfenster erscheinen:

```
Geparstes Datum mit DateValue: 1.4.2023
```

Beachten Sie, dass der Erfolg des Parsens davon abhängt, dass das Datumsformat des Strings mit den System- oder Anwendungseinstellungen übereinstimmt.

## Vertiefung

Intern verwendet VBA bei der Umwandlung eines Strings in ein Datum die regionalen Einstellungen des Windows-Betriebssystems, um das Datumsformat zu interpretieren. Dies zu verstehen ist entscheidend, da ein Datumstring, der auf einem System perfekt geparst wird, auf einem anderen ein Fehler verursachen könnte, wenn sie unterschiedliche Datum-/Zeiteinstellungen verwenden.

Historisch gesehen war die Handhabung von Daten eine häufige Quelle von Bugs in Anwendungen, insbesondere in solchen, die international verwendet werden. Diese Abhängigkeit von regionalen Einstellungen in VBA ist der Grund, warum einige möglicherweise Alternativen wie das ISO 8601-Format (z. B. "JJJJ-MM-TT") für eine eindeutige Datumsrepräsentation und Parsing über verschiedene Systeme hinweg in Betracht ziehen. Leider unterstützt VBA ISO 8601 nicht nativ, und für eine strikte Einhaltung wäre ein manuelles Parsen erforderlich.

Für komplexes Datumparsen, das über das hinausgeht, was `CDate` oder `DateValue` bewältigen können, oder um ein konsistentes Parsing unabhängig von den Systemlokaleinstellungen zu gewährleisten, könnten Programmierer auf benutzerdefinierte Parsefunktionen zurückgreifen. Diese könnten das Aufteilen des Datumstrings in Komponenten (Jahr, Monat, Tag) und die Konstruktion eines Datums mit der Funktion `DateSerial` beinhalten. Andere könnten sich für leistungsfähigere Sprachen oder Bibliotheken entscheiden, die mit Internationalisierung im Sinn konzipiert wurden, für solche Aufgaben.
