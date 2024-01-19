---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Drucken von Debug-Ausgaben ist ein Prozess, bei dem Programmierer die Innenarbeit ihres Codes verfolgen. Das hilft uns, Fehler zu finden und zu korrigieren. 

## Wie macht man das:
PowerShell macht das einfach. Benutze den `Write-Debug` Befehl. Hier ist ein einfaches Beispiel:

```PowerShell
# Aktivieren Sie zuerst die Debug-Preference
Set-PSDebug -Trace 1

# Dann docke Debug-Ausgabe
Write-Debug "Das ist eine Debug-Nachricht"

# Die Debug-Nachricht wird angezeigt
debug: Das ist eine Debug-Nachricht
```

## Tiefer eintauchen
Historisch gesehen, Debugging wurde seit den frühen Tagen der Programmierung verwendet. In PowerShell, `Write-Debug` ist nicht die einzige Option. Alternativen beinhalten `Write-Verbose` oder `Write-Information`.

Die Implementierung von Debug-Ausdrucken in PowerShell hängt von der `PSDebug`-Preference ab. Mit `Set-PSDebug -Trace 1` aktivieren Sie eine detaillierte Spur aller ausgeführten Befehlslinien.

## Siehe auch
Für weitere Informationen und Beispiele, schauen Sie sich diese Ressourcen an:

- [PowerShell-Dokumentation zu Write-Debug](http://go.microsoft.com/fwlink/?LinkID=113426)
- [Set-PSDebug-Dokumentation](http://go.microsoft.com/fwlink/?linkid=2116527)
- [Artikel über alternative Debugging-Methoden in PowerShell](https://devblogs.microsoft.com/scripting/weekend-scripter-the-many-ways-to-use-powershell/)