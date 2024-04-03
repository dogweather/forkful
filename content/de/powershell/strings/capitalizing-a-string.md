---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:57.057018-07:00
description: "Wie: PowerShell, als vielseitiges Werkzeug, erm\xF6glicht es Ihnen,\
  \ einen String auf einfache Weise zu gro\xDFzuschreiben, ohne dass Drittanbieterbibliotheken\u2026"
lastmod: '2024-03-13T22:44:54.084885-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, als vielseitiges Werkzeug, erm\xF6glicht es Ihnen, einen String\
  \ auf einfache Weise zu gro\xDFzuschreiben, ohne dass Drittanbieterbibliotheken\
  \ ben\xF6tigt werden."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
PowerShell, als vielseitiges Werkzeug, ermöglicht es Ihnen, einen String auf einfache Weise zu großzuschreiben, ohne dass Drittanbieterbibliotheken benötigt werden. So können Sie es machen:

```powershell
# Verwenden der eingebauten .Net-Methode 'ToTitleCase' von CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Ausgabe:
```
Hello world
```

Hinweis: Diese Methode setzt den ersten Buchstaben jedes Wortes groß. Wenn Sie strikt nur den ersten Buchstaben des Strings großschreiben und den Rest unverändert lassen möchten, könnten Sie so etwas machen:

```powershell
# Nur das erste Zeichen eines Strings großschreiben
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Ausgabe:
```
Hello world
```

PowerShell enthält direkt keine einfache Funktion zum Großschreiben nur des ersten Buchstabens eines Strings, aber durch die Kombination der grundlegenden Methoden zur Stringmanipulation wie `Substring(0,1).ToUpper()` und Verkettung können wir leicht das gewünschte Ergebnis erreichen.
