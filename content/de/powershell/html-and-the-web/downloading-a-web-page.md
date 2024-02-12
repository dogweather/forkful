---
title:                "Webseite herunterladen"
aliases: - /de/powershell/downloading-a-web-page.md
date:                  2024-01-20T17:44:50.030093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/downloading-a-web-page.md"
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
