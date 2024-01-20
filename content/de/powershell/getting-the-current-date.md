---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums ist ein grundlegender Aspekt in der Programmierung, durch den wir Zugang zu dem genauen Zeitpunkt der Ausführung einer bestimmten Code-Zeile haben. Es spielt eine unverzichtbare Rolle in Bereichen wie Protokollierung, Terminplanung und Zeitstempel für Transaktionen.

## So geht's:

Um das aktuelle Datum in PowerShell zu ermitteln, verwenden wir das Cmdlet `Get-Date`.

```PowerShell
$datum = Get-Date
$datum
```

Als Ausgabe bekommen Sie das aktuelle Datum und die Uhrzeit:

```PowerShell
Samstag, 12. November 2022 08:05:42
```

## Deep Dive:

Historisch gesehen wurde in PowerShell das Cmdlet `Get-Date` eingeführt, um das Datum und die Uhrzeit abzurufen. Es gibt auch Alternativen, wie die `.NET`-Methode `[DateTime]::Now`, aber in den meisten Fällen ist `Get-Date` die bevorzugte Lösung.

Im Hintergrund ruft `Get-Date` die `[DateTime]::Now`-.NET Methode auf, um das aktuelle Datum und die Uhrzeit abzurufen. Es bietet auch zusätzliche Optionen, um das Format der zurückgegebenen Zeichenkette zu kontrollieren. Beispiel:

```PowerShell
$heutigesDatum = Get-Date -Format "dd-MM-yyyy"
$heutigesDatum
```

Ausgabe:

```PowerShell
12-11-2022
```

## Siehe Auch:

1. Offizielle PowerShell-Dokumentation über `Get-Date` - (https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)

2. Microsoft .NET-Dokumentation `DateTime.Now` - (https://docs.microsoft.com/de-de/dotnet/api/system.datetime.now?view=net-5.0)

3. Führung durch die grundlegenden Cmdlets in PowerShell - (https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-day-day-sysadmin-use/)