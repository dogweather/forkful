---
title:                "Einen neuen Projekt starten"
aliases:
- /de/powershell/starting-a-new-project.md
date:                  2024-01-20T18:04:46.693913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu beginnen bedeutet, eine frische Arbeitsumgebung für eine bestimmte Aufgabe zu schaffen. Programmierer tun dies, um ihre Ideen zu strukturieren, Abhängigkeiten zu verwalten und einen klaren Ausgangspunkt für die Entwicklung zu haben.

## How to:
Um ein neues PowerShell-Projekt anzufangen, beginnen wir oft mit der Erstellung eines Verzeichnisses. Dann initialisieren wir ein Git-Repository und erstellen eine .ps1-Datei für unser Skript.

```PowerShell
# Erstelle ein neues Verzeichnis für das Projekt
New-Item -Path "C:\MeinProjekt" -ItemType Directory

# Wechsel in das neue Verzeichnis
Set-Location -Path "C:\MeinProjekt"

# Initialisiere ein neues Git-Repository
git init

# Erstelle eine neue PowerShell-Script-Datei
New-Item -Path ".\MeinSkript.ps1" -ItemType File
```

## Deep Dive:
Der Prozess des Projektstarts hat sich seit den frühen Tagen der Programmierung weiterentwickelt. Früher wurden Projekte oft ohne Versionskontrolle oder definierte Struktur angefangen. Heute gilt Git als de-facto Standard für Versionskontrolle und Projekte profitieren enorm durch seine Verwendung.

Eine Alternative zum manuellen Erstellen von Verzeichnissen und Dateien ist die Verwendung von Projektmanagern wie `PSModuleDevelopment` oder Build-Automatisierungstools wie `psake`, die sich um die Einrichtung und Verwaltung von Projektabhängigkeiten kümmern.

In der Praxis hängt es davon ab, was das Projektziel ist. PowerShell-Module benötigen beispielsweise eine `.psd1`-Manifestdatei, während Skripte oft nur eine `.ps1`-Datei sind. Größere Projekte profitieren von einer Dateistruktur, in der Skripte, Funktionen und Tests in separate Dateien oder Verzeichnisse aufgeteilt sind.

## See Also:
- [Über das Erstellen von PowerShell-Modulen](https://docs.microsoft.com/de-de/powershell/scripting/developer/module/how-to-write-a-powershell-module-manifest)
- [Git – ein einfacher Einstieg](https://git-scm.com/book/de/v2/Erste-Schritte-Git-grundlegend-verstehen)
- [Projektmanagement in PowerShell mit psake](https://github.com/psake/psake)
- [PSModuleDevelopment auf PowerShell Gallery](https://www.powershellgallery.com/packages/PSModuleDevelopment)
