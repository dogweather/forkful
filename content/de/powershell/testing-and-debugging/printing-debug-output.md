---
date: 2024-01-20 17:53:04.756883-07:00
description: "Druckausgaben zum Debuggen sind so etwas wie Wegweiser beim Programmieren\
  \ - sie zeigen, wo die Daten entlanglaufen und wo sie sich verstecken.\u2026"
lastmod: '2024-03-11T00:14:28.006113-06:00'
model: gpt-4-1106-preview
summary: "Druckausgaben zum Debuggen sind so etwas wie Wegweiser beim Programmieren\
  \ - sie zeigen, wo die Daten entlanglaufen und wo sie sich verstecken.\u2026"
title: Debug-Ausgaben drucken
---

{{< edit_this_page >}}

## Was & Warum?
Druckausgaben zum Debuggen sind so etwas wie Wegweiser beim Programmieren - sie zeigen, wo die Daten entlanglaufen und wo sie sich verstecken. Programmierer nutzen sie, um den Überblick zu behalten, denken wir mal an Brotkrumen im Wald der Variablen und Funktionen.

## Wie geht das:
PowerShell bietet das `Write-Host`, `Write-Debug`, `Write-Verbose` und `Write-Information` Cmdlet an, um unterschiedliche Arten von Ausgaben zu erzeugen. Einfach im Code an der gewünschten Stelle platziert:

```PowerShell
# Einfache Ausgabe
Write-Host "Hier stehen Dinge, die jeder sehen soll."

# Debug-Ausgabe
Write-Debug "Dies sehen nur die, die es wirklich wollen."

# Ausführliche Ausgabe
Write-Verbose "Falls jemand JEDEN Schritt wissen möchte."

# Informationen
Write-Information "Hier steht etwas Wichtiges, vielleicht."
```

Standardausgabe von `Write-Host` wird direkt sichtbar sein. Um die anderen Ausgaben zu sehen, müsst ihr die entsprechenden Präferenzen setzen ($DebugPreference, $VerbosePreference, etc.) oder die Parameter -Debug und -Verbose beim Scriptaufruf nutzen.

## Vertiefung:
Früher war `Write-Host` verpönt, weil es die Ausgabe direkt in die Konsole schrieb, ohne Umleitungsmöglichkeiten. Ab PowerShell 5.0 ist das anders - jetzt lässt sich alles mit `Write-Host` Geschriebene umleiten oder aufzeichnen. Alternativen sind die Verwendung von `Write-Output` für normale Outputs oder `Out-File` zum Schreiben in Dateien. Der Hauptunterschied liegt in der Sichtbarkeit und dem Umgang mit den Ausgaben im weiteren Verlauf: `Write-Host` ist eher dann, wenn es nicht weiterverarbeitet werden muss, `Write-Output` hingegen ist perfekt, wenn Ergebnisse weitergeleitet oder genutzt werden sollen.

## Siehe auch:
- Ein Artikel zu den Best Practices: [PowerShell Best Practices](https://devblogs.microsoft.com/scripting/understanding-streams-redirection-and-write-host-in-powershell/)
