---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "PowerShell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Lesen von Befehlszeilenargumenten in PowerShell ist ein wichtiger Teil beim Programmieren. Dabei werden die Argumente, die einem Skript oder einer Anwendung beim Ausführen übergeben werden, eingelesen und verarbeitet. Programmierer machen das, um Benutzern die Möglichkeit zu geben, Einstellungen oder Aktionen beim Starten eines Programms anzupassen oder anzugeben.

## Wie geht's?
In PowerShell gibt es das automatische Argument-Array `$args`, in dem alle Befehlszeilenargumente gespeichert werden. Zum Beispiel:
```PowerShell
# Beispielaufruf: powershell.exe -file "C:\MeinSkript.ps1" Argument1 Argument2

$script = $args[0] # $script enthält "C:\MeinSkript.ps1"
$arg1 = $args[1] # $arg1 enthält "Argument1"
$arg2 = $args[2] # $arg2 enthält "Argument2"

Write-Host "Skriptpfad: $script"
Write-Host "Erstes Argument: $arg1"
Write-Host "Zweites Argument: $arg2"
```
Die Ausgabe wäre:
```
Skriptpfad: C:\MeinSkript.ps1
Erstes Argument: Argument1
Zweites Argument: Argument2
```

## Tiefentauchen
Das Einlesen von Befehlszeilenargumenten ist eine gängige Praxis in der Programmierung, die bereits seit den Anfängen der Kommandozeilen-Interfaces verwendet wird. In anderen Skriptsprachen wie Python oder Perl ist es ebenfalls üblich, Befehlszeilenargumente zu verarbeiten. Eine Alternative zu `$args` in PowerShell ist die spezifische Parameter-Deklaration in einem Skript, um benannte Argumente anzunehmen. Implementierungsdetails können je nach Plattform und Skriptsprache unterschiedlich sein.

## Siehe auch
- Offizielle Dokumentation zu Befehlszeilenargumenten in PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_command_arguments?view=powershell-7.1
- Blog-Beitrag über Befehlszeilenargumente in PowerShell Skripten: https://adamtheautomator.com/powershell-command-line-arguments/