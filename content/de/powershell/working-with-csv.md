---
title:                "Arbeiten mit CSV"
html_title:           "PowerShell: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

Du hast wahrscheinlich schon von CSV gehört, aber was genau ist das eigentlich? CSV steht für "Comma Separated Values" und ist ein Dateiformat für tabellenartige Daten. CSV-Dateien können in Tabellenkalkulationsprogrammen wie Excel geöffnet werden und werden auch häufig von Programmierern verwendet.

Aber warum benutzen Programmierer CSV? Nun, CSV ist ein sehr einfaches Format, in dem Daten in einer einfachen Textdatei gespeichert werden können. Es ist plattformübergreifend und kann einfach von verschiedenen Programmen gelesen und geschrieben werden. CSV-Dateien sind auch relativ klein und einfach zu manipulieren, was sie zu einer idealen Wahl für das Lesen und Schreiben von Daten in PowerShell macht.

## So geht's:

Um mit CSV in PowerShell zu arbeiten, gibt es ein paar nützliche Cmdlets, die du kennen solltest:

```PowerShell
Import-Csv -Path "Pfad zur CSV-Datei" | Format-Table
```
Dieses Cmdlet importiert die CSV-Datei und stellt sie in einer übersichtlichen Tabelle dar.

```PowerShell
Export-Csv -Path "Pfad zur Ziel-CSV-Datei" -NoTypeInformation
```
Dieses Cmdlet exportiert Daten aus PowerShell in eine CSV-Datei und entfernt automatisch die Typinformationen.

```PowerShell
ConvertFrom-Csv -Delimiter ";" -Header "Spalte1", "Spalte2" -Encoding UTF8
```
Dieses Cmdlet konvertiert die Daten aus einer CSV-Datei in PowerShell-Objekte und ermöglicht es dir, die Trennzeichen und Überschriften anzupassen.

## Tief tauchen:

CSV gibt es schon seit den 1970er Jahren und ist immer noch ein weit verbreitetes Dateiformat für den Austausch von Daten. Es ist besonders hilfreich bei der Arbeit mit großen Datenmengen oder der Integration von Daten aus verschiedenen Quellen.

Es gibt auch alternative Dateiformate für tabellenartige Daten, wie zum Beispiel Excel (XLSX), JSON oder XML. Diese haben ihre eigenen Stärken und Schwächen, aber CSV bleibt eine beliebte Wahl aufgrund seiner Einfachheit.

In PowerShell werden CSV-Dateien standardmäßig als UTF-8-Dateien erstellt, was für die meisten Fälle ausreichend ist. Wenn du jedoch Sonderzeichen oder Symbole in deinen Daten hast, solltest du die Codierung mit dem Parameter "-Encoding" anpassen.

## Siehe auch:

- [PowerShell-Dokumentation zu CSV](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv?view=powershell-7.1)
- [Alternativen zu CSV: Excel, JSON und XML](https://medium.com/@bcorcos/why-i-still-use-csv-3b113736df8f)