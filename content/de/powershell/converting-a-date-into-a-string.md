---
title:                "Das Umwandeln eines Datums in einen String"
html_title:           "PowerShell: Das Umwandeln eines Datums in einen String"
simple_title:         "Das Umwandeln eines Datums in einen String"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Konvertieren von Datum in einen String ist ein wichtiger Teil der Programmierung, der es ermöglicht, Datum und Uhrzeit in einem menschenlesbaren Format darzustellen. Programmierer machen dies, um Daten für den Benutzer verständlicher zu machen oder um sie in einem bestimmten Format für die Verarbeitung durch das Programm zu haben.

## Anleitung:
Mit PowerShell ist es einfach, ein Datum in einen String zu konvertieren. Verwenden Sie einfach den Befehl `Get-Date` gefolgt von der Methode `.ToString()` und dem gewünschten Format. Zum Beispiel:
```
Get-Date .ToString("dd.MM.yyyy")
```
Dies gibt das aktuelle Datum im Format "Tag.Monat.Jahr" zurück. Wenn Sie ein bestimmtes Datum konvertieren möchten, können Sie auch den Parameter `-Date` verwenden. Zum Beispiel:
```
Get-Date -Date "2020-10-08" .ToString("dddd")
```
Dies gibt den Wochentag des 8. Oktober 2020 zurück.

## Tiefer Einblick:
Das Konzept der Konvertierung von Datum in einen String gibt es schon seit langer Zeit. Früher mussten Programmierer viel mehr Code schreiben, um dieses Konvertierungsverfahren durchzuführen. Heutzutage macht PowerShell dies viel einfacher und flexibler, da es eine integrierte Methode `ToString()` bietet.

Es gibt auch alternative Möglichkeiten, ein Datum in einen String umzuwandeln, wie z.B. die Verwendung von String-Formatierung mit dem Ampersand-Operator `&` oder der Methode `.format()`.

Bei der Implementierung dieser Methode ist es wichtig zu beachten, dass das gewählte Format korrekt angegeben wird, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch:
Für weitere Informationen zum Konvertieren von Datum in einen String können Sie sich die offiziellen PowerShell-Dokumentationen ansehen:
[https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)

Es gibt auch viele nützliche Beispiele und Tutorials online, die Ihnen bei der Verwendung von PowerShell für die Konvertierung von Datum in einen String helfen können. Einige davon sind:
- [https://www.tutorialspoint.com/powershell/powershell_date_time.htm](https://www.tutorialspoint.com/powershell/powershell_date_time.htm)
- [https://adamtheautomator.com/powershell-converting-dates-to-strings/](https://adamtheautomator.com/powershell-converting-dates-to-strings/)