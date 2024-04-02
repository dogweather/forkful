---
date: 2024-01-26 00:55:37.846233-07:00
description: "Fehler in PowerShell zu behandeln bedeutet, Missgeschicke zu antizipieren\
  \ und sie reibungslos zu verwalten. Programmierer machen dies, um Abst\xFCrze zu\u2026"
lastmod: '2024-03-13T22:44:54.111362-06:00'
model: gpt-4-1106-preview
summary: "Fehler in PowerShell zu behandeln bedeutet, Missgeschicke zu antizipieren\
  \ und sie reibungslos zu verwalten. Programmierer machen dies, um Abst\xFCrze zu\u2026"
title: Fehlerbehandlung
weight: 16
---

## Was & Warum?
Fehler in PowerShell zu behandeln bedeutet, Missgeschicke zu antizipieren und sie reibungslos zu verwalten. Programmierer machen dies, um Abstürze zu verhindern und den Nutzern hilfreiches Feedback zu geben.

## Wie geht das:
```PowerShell
# Grundlegender Try-Catch, um Ausnahmen zu behandeln
try {
    # Code, der einen Fehler auslösen könnte
    $result = 1 / 0
} catch {
    # Was tun, wenn ein Fehler aufgetreten ist
    Write-Host "Ups, ein Fehler ist aufgetreten: $_"
}

# Ausgabe einer benutzerdefinierten Fehlermeldung
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Die Datei konnte nicht gefunden werden."
}

# Verwenden der $Error-Variable, um den letzten Fehler zu untersuchen
```
## Vertiefung
PowerShell hat seit seiner Einführung als Monad einen langen Weg zurückgelegt. Die Fehlerbehandlung wurde im Laufe der Zeit robuster und bot Funktionen, die ähnlich wie in anderen Programmiersprachen sind. Die `try-catch-finally`-Syntax ist eine solche Übernahme aus Sprachen wie C#. Davor waren Skriptersteller stark darauf angewiesen, Bedingungen zu prüfen und die `$Error`-Automatikvariable zu verwenden.

PowerShell hat auch zwei Haupttypen von Fehlern: beendende und nicht beendende. Beendende Fehler werden das Skript anhalten, es sei denn, sie werden in einem `try-catch`-Block gefangen, während nicht beendende Fehler das nicht tun, es sei denn, man gibt `-ErrorAction Stop` an. Diese Unterscheidung ist entscheidend, da sie eine feine Kontrolle über die Fehlerbehandlung gewährt, indem entschieden wird, ob ein Fehler wirklich das Anhalten des gesamten Skripts rechtfertigt oder einfach protokolliert und ignoriert werden kann.

Die Fehlerbehandlung in PowerShell erlaubt auch einen `finally`-Block, der in jedem Fall ausgeführt wird - egal, ob ein Fehler aufgetreten ist oder nicht. Er ist ideal für Aufräumarbeiten.

Wenn man tief in den Skripting-Gräben steckt, kann man auch spezifische Ausnahmetypen behandeln, was noch feinere Kontrolle ermöglicht.

Alternativ gibt es den altbewährten `-ErrorVariable`-Parameter, um Fehler zu erfassen, ohne eine Ausnahme auszulösen. Und die `$?`-Variable teilt Ihnen mit, ob die letzte Operation erfolgreich war. Sie sind praktische Werkzeuge, wenn auch etwas weniger sauber als ein solides `try-catch`.

## Siehe Auch
- [about_Try_Catch_Finally](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
