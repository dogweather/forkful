---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
aliases: - /de/powershell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:41.411313-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums in PowerShell bedeutet einfach, bestimmte Zeitspannen zu einem existierenden Datum hinzuzufügen oder davon zu subtrahieren. Programmierer verwenden diese Funktion für Aufgaben wie Fälligkeitsberechnungen, Zeitmessungen und Planungen.

## So geht's:
```PowerShell
# Datum in der Zukunft berechnen
$heute = Get-Date
$zukunft = $heute.AddDays(10)
$zukunft.ToString("dd.MM.yyyy")
```
Ausgabe: `19.04.2023` (angenommen, heute ist der 9. April 2023)

```PowerShell
# Datum in der Vergangenheit berechnen
$vergangenheit = $heute.AddDays(-10)
$vergangenheit.ToString("dd.MM.yyyy")
```
Ausgabe: `30.03.2023`

## Deep Dive:
PowerShell verwendet das `[DateTime]`-Objekt, um mit Daten und Zeiten zu arbeiten. Diese Funktionalität gibt es schon seit den Anfangstagen von .NET. Alternativen zur `AddDays`-Methode sind zum Beispiel `AddMonths`, `AddYears`, `AddHours` usw., je nach Bedarf. 

Die Methoden sind Teil der .NET-Klassenbibliothek und wurden für PowerShell angepasst, was die Sprache besonders mächtig für solche Operationen macht. Hinter den Kulissen arbeiten diese Methoden mit Ticks, die kleinste Zeitmessung in .NET, die 100 Nanosekunden entspricht. Das macht Berechnungen sehr präzise.

## See Also:
- [Microsoft-Dokumentation zur .NET DateTime-Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
