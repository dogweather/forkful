---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Analyse eines Datums aus einer Zeichenkette ist eine gängige Aufgabe, bei der Zeichen in ein bestimmtes Datum umgewandelt werden. Entwickler führen diese Aufgabe durch, um Datumseinträge in menschenlesbaren Formaten zu lesen und zu verarbeiten.

## So macht man's:

Um ein Datum aus einer Zeichenkette in PowerShell zu analysieren, verwenden wir die Methode `[DateTime]::ParseExact()`. Hier ist ein einfaches Beispiel:

```PowerShell
$rawDate = "2021-12-31"
$format = "yyyy-MM-dd"

$parsedDate = [DateTime]::ParseExact($rawDate, $format, $null)

Write-Output $parsedDate
```
Ausgabe:
```
Freitag, 31. Dezember 2021 00:00:00
```
Das Beispiel liest das Datum in der Zeichenkette `rawDate` mit dem angegebenen Format `format` und gibt dann das analysierte Datum aus.

## Vertiefen

Die Methode `[DateTime]::ParseExact()` wurde in früheren Versionen von PowerShell eingeführt und wird bis heute wegen ihrer Flexibilität und Genauigkeit geschätzt. Eine Alternative zu `ParseExact` wäre die Verwendung der Methode `Parse`, die flexible Datumsformate behandelt, allerdings mit weniger Kontrolle. Im Detail arbeitet `ParseExact` durch das Abgleichen der Eingabezeichenkette mit dem bereitgestellten Format und wirft einen Fehler, wenn das Format nicht übereinstimmt.

## Siehe auch

Für weitere Informationen und Beispiele zum Analysieren von Daten aus Zeichenketten in PowerShell, siehe die offizielle Microsoft-Dokumentation:
- [DateTime.ParseExact Methode](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.parseexact?view=net-5.0)
- [DateTime.Parse Methode](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.parse?view=net-5.0)
- [Dokumentation zu PowerShell-Datumsformaten](https://docs.microsoft.com/de-de/dotnet/standard/base-types/custom-date-and-time-format-strings)