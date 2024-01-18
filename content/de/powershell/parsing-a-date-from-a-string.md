---
title:                "Datum aus einer Zeichenkette analysieren"
html_title:           "PowerShell: Datum aus einer Zeichenkette analysieren"
simple_title:         "Datum aus einer Zeichenkette analysieren"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist die Fähigkeit, ein bestimmtes Format in einem String zu erkennen und es in ein lesbares Datumsformat umzuwandeln. Programmierer nutzen dieses Feature, um beispielsweise Datumsangaben aus Textdateien zu extrahieren oder Benutzereingaben in einem bestimmten Format zu akzeptieren.

## Wie geht's?

Um ein Datum aus einem String zu parsen, kann man die Methode ```ParseExact``` aus PowerShell nutzen. Diese Methode erwartet zwei Parameter: den String mit dem Datum und das gewünschte Datumsformat. Ein Beispiel sieht so aus:

```PowerShell
$dateString = "20.03.2020"
$format = "dd.MM.yyyy"
[DateTime]::ParseExact($dateString, $format, $null)
```

Das Ergebnis wird als Objekt vom Typ ```DateTime``` ausgegeben und kann dann weiterverarbeitet werden.

## Tiefere Einblicke

Die Notwendigkeit, ein Datum aus einem String zu parsen, kommt daher, dass verschiedene Länder und Regionen unterschiedliche Datumsformate verwenden. Beispielsweise verwenden die USA das Monat-Tag-Jahr-Format, während viele europäische Länder das Tag-Monat-Jahr-Format bevorzugen.

Alternativ zum Parsen kann man auch reguläre Ausdrücke nutzen, um ein Datum aus einem String zu extrahieren. Jedoch ist dies oft nicht zuverlässig, da es schwierig ist, alle möglichen Datumsformate abzudecken.

Die Implementierung von Datums-Parsing in PowerShell basiert auf der Klasse ```System.DateTime```, die verschiedene Methoden und Eigenschaften zur Manipulation von Datumsangaben bietet.

## Siehe auch

- Die offizielle Microsoft-Dokumentation zu [DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=netframework-4.8)
- Eine Übersicht über [Datumsformate in verschiedenen Ländern](https://en.wikipedia.org/wiki/Date_format_by_country)