---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:47.984087-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist einfach das Erfragen des jetzigen Tages nach Jahr, Monat und Tag. Programmierer nutzen diesen Befehl, um Zeitstempel zu setzen, tägliche Tasks zu planen oder um zeitabhängige Funktionen auszuführen.

## How to:
Der einfachste Weg, das aktuelle Datum in PowerShell zu erhalten, ist der `Get-Date` Befehl. Hier ein paar Beispiele:

```PowerShell
# Hol dir das komplette aktuelle Datum und die Uhrzeit
Get-Date

# Formatierung auf das Datum beschränken
Get-Date -Format "yyyy-MM-dd"

# Nur die Uhrzeit erhalten
Get-Date -Format "HH:mm:ss"
```

Sample Output:

```PowerShell
# Beispiel für komplettes Datum und Uhrzeit
Freitag, 21. April 2023 15:41:34

# Beispiel für reines Datum
2023-04-21

# Beispiel für reine Uhrzeit
15:41:34
```

## Deep Dive:
PowerShell verwendet `Get-Date` seit seiner Entstehung. Es ist eine Anpassung früherer Kommandozeilenfunktionen, die sich über die Jahre weiterentwickelt hat. 

Alternativen:
- `System.DateTime` Klasse aus .NET in PowerShell verwenden.
- Windows Command Line nutzen (`time` oder `date` Befehle), allerdings weniger flexibel.

Implementierungsdetails:
`Get-Date` ruft Informationen vom System über .NET-Funktionen ab. Du kannst es mit weiteren Parametern wie `-UFormat` für UNIX-Formatierung oder `-DisplayHint` verwenden, um gezielt Datum, Uhrzeit oder beides anzuzeigen.

## See Also:
- PowerShell-Dokumentation zu `Get-Date`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date
- .NET DateTime-Klasse: https://docs.microsoft.com/dotnet/api/system.datetime
- Formatierung von Datum und Uhrzeit in PowerShell: https://docs.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-datetime
