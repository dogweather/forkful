---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung eines Datums in eine Zeichenkette (String) ermöglicht es Programmierern, Daten auf eine lesbarere Art zu präsentieren und zu manipulieren. Dies ist besonders nützlich, wenn Sie Daten für eine benutzerfreundliche Anzeige formatieren oder Daten für den Export und die weitere Verarbeitung vorbereiten.

## So geht's:
PowerShell macht die Umwandlung eines Datums in einen String zum Kinderspiel. Hier ist ein einfaches Beispiel:

```PowerShell
# Ein Datum erstellen
$datum = Get-Date
Write-Output $datum
```
Dies gibt das aktuelle Datum und die aktuelle Uhrzeit aus. Um es in einen String umzuwandeln, verwenden wir die Methode `ToString()`:

```PowerShell
# Datum in String umwandeln
$datumString = $datum.ToString()
Write-Output $datumString
```
Die Ausgabe bleibt gleich, der Typ des Objekts hat sich jedoch in einen String geändert.

## Vertiefung
PowerShell basiert auf .NET, daher können Sie auf eine Vielzahl von Methoden zum Konvertieren und Formatieren von Datum und Uhrzeit zurückgreifen. Die Methode `ToString()` kann mit einem Formatstring erweitert werden, um die Ausgabe des Datums anzupassen. Zum Beispiel gibt `$datum.ToString("yyyy-MM-dd")` das Datum im Format 2023-04-01 aus. 

Eine Alternative zur `ToString()` Methode ist die `Format` Methode, die ähnlich funktioniert und weitere Formatierungsoptionen bietet. Zum Beispiel liefert `$datum.Format("D")` das Datum in einem ausführlichen Format, z.B. "Samstag, 1. April 2023".

Die Behandlung von Zeitzonen und anderen Komplexitäten des Datumsformates kann schwierig sein. Aber mit ausreichender Kenntnis und Sorgfalt ist es möglich, jedes Datumsformat zu erzeugen, das man benötigt.

## Siehe auch
Weitere Informationen über Datumsformatierung und Umwandlung finden Sie in der PowerShell-Dokumentation:
- Datums- und Uhrzeitformate in .NET: https://docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings
- Umwandlungsmethoden und Klassen in .NET: https://docs.microsoft.com/de-de/dotnet/api/system.convert?view=net-5.0