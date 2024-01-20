---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben in den Standardfehler (stderr) ist eine Methode, um Fehlermeldungen getrennt von normalen Ausgabedaten zu behandeln. Das hilft beim Debuggen und bei der Log-Analyse, weil Fehler so einfacher zu erkennen und zu filtern sind.

## How to:
Um in PowerShell in den Standardfehler zu schreiben, verwendet man `Write-Error` oder man leitet die Ausgabe mit `1>&2` um. Hier einige Beispiele:

```PowerShell
# Schreibt eine Fehlermeldung direkt in stderr
Write-Error "Das ist ein Fehler"

# Leitet eine Ausgabe in stderr um
"Fehler gefunden" 1>&2
```

Output:
```
Write-Error : Das ist ein Fehler
Fehler gefunden
```

## Deep Dive:
Das Schreiben in den Standardfehler gibt es schon seit den frühen Tagen der Unix-Shell. PowerShell macht sich dieses Konzept zunutze, um eine verbesserte Fehlerbehandlung zu ermöglichen. Anders als auf Unix-Systemen wirft `Write-Error` standardmäßig keinen Fehler im Sinne eines Exit-Status'. Alternativen zum Schreiben in stderr in PowerShell könnten das Loggen in Dateien oder die Verwendung von `Write-Host` für nicht-terminale Ausgaben sein, obwohl letzteres nicht empfohlen wird, da es die Trennung von Output und Fehlern aufhebt. In Skripten sorgt `Throw` für die Erzeugung einer terminierenden Ausnahme, die das Skript stoppt, und ist daher für ernsthafte Fehlerbehandlungen geeignet.