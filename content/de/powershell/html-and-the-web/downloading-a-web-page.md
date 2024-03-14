---
date: 2024-01-20 17:44:50.030093-07:00
description: "Herunterladen einer Webseite bedeutet, ihren Inhalt zu sichern, um ihn\
  \ offline zu analysieren oder zu verarbeiten. Programmierer machen das, um Daten\
  \ zu\u2026"
lastmod: '2024-03-13T22:44:54.102484-06:00'
model: gpt-4-1106-preview
summary: "Herunterladen einer Webseite bedeutet, ihren Inhalt zu sichern, um ihn offline\
  \ zu analysieren oder zu verarbeiten. Programmierer machen das, um Daten zu\u2026"
title: Webseite herunterladen
---

{{< edit_this_page >}}

## What & Why?
Herunterladen einer Webseite bedeutet, ihren Inhalt zu sichern, um ihn offline zu analysieren oder zu verarbeiten. Programmierer machen das, um Daten zu sammeln, automatisierte Tests durchzuführen oder Inhalte zu überwachen.

## How to:
PowerShell macht's einfach. Hier ein Basic-Beispiel, das eine Webseite lädt:

```PowerShell
$response = Invoke-WebRequest -Uri 'http://example.com'
$response.Content
```

Ausgabe könnte so aussehen:

```HTML
<!doctype html>
....
</html>
```

Erweitertes Beispiel, mit Speichern des Inhalts in einer Datei:

```PowerShell
$url = 'http://example.com'
$outputPath = 'C:\path\to\your\file.html'
Invoke-WebRequest -Uri $url -OutFile $outputPath
Write-Host "Webseite heruntergeladen nach: $outputPath"
```

## Deep Dive:
Vor PowerShell nutzte man Tools wie `wget` oder `curl` für solche Aufgaben. PowerShell bietet nun mit `Invoke-WebRequest` und `Invoke-RestMethod` eigene Befehle. `Invoke-WebRequest` eignet sich für Standard-HTTP-Anfragen, während `Invoke-RestMethod` für den Umgang mit REST-APIs gedacht ist.

Anders als `curl`, das mit Command-Line-Optionen arbeitet, nutzt PowerShell's `Invoke-WebRequest` benannte Parameter und gibt Objekte zurück. Das ermöglicht eine tiefere Integration in Skripte und komplexere Verarbeitung ohne zusätzlichen Parsing-Aufwand.

## See Also:
- [PowerShell Dokumentation für Invoke-WebRequest](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Mehr zu REST-APIs mit Invoke-RestMethod](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-restmethod)
