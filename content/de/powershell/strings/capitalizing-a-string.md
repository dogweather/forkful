---
title:                "Einen String großschreiben"
aliases:
- /de/powershell/capitalizing-a-string.md
date:                  2024-02-03T19:05:57.057018-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings in PowerShell beinhaltet die Umwandlung des ersten Zeichens eines gegebenen Strings in Großbuchstaben, während der Rest des Strings unverändert bleibt. Programmierer führen diese Aufgabe oft für Formatierungszwecke durch, wie zum Beispiel die Vorbereitung von Texten für die Anzeige in Benutzeroberflächen oder das Befolgen von grammatikalischen Regeln in generierten Dokumenten.

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
