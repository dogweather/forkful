---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings ist das Herausschneiden kleinerer Zeichenketten aus einer größeren Zeichenkette. Programmierer machen das, um spezifische Daten zu isolieren und zu vereinfachen.

## Wie es geht:

Mit PowerShell können wir Teilstrings auf verschiedene Weisen extrahieren. Hier sind einige Beispiele:

```PowerShell
# Ein Beispielstring
$string = "Hallo Welt"
 
# Extrahieren des ersten Teilstrings
$teilstring1 = $string.Substring(0,5)
Write-Output $teilstring1
```
Output: `Hallo`

```PowerShell
# Extrahieren des zweiten Teilstrings
$teilstring2 = $string.Substring(6)
Write-Output $teilstring2
```
Output: `Welt`

## Vertiefung

In historischer Hinsicht ist das Extrahieren von Teilstrings eine grundlegende Fähigkeit in der Programmierung und existiert bereits seit den frühen Tagen von COBOL und FORTRAN.

Alternativ können wir das '-'Operator in Kombination mit dem `split`-Befehl verwenden, um einen String zu teilen und Teilstrings zu erstellen. Um die Implementierung zu optimieren, benutzen wir in den meisten Fällen jedoch die gängige Methode, die `Substring`-Methode.

```PowerShell
# Ein Beispiel mit dem '-'Operator und "split"-Befehl
$string = "Hallo-Welt"
$teilstrings = $string -split "-"
Write-Output $teilstrings
```
Output: `Hallo`, `Welt`

Beachte bitte, dass wir bei Verwendung der `Substring`-Methode mit 0-Anfangen zu zählen, während wir bei der '-split' Methode, jeden Teilstring als separates Element behandeln.

## Siehe auch:

- Mehr Anwendungsbeispiele für das Extrahieren von Teilstrings in PowerShell: https://docs.microsoft.com/de-de/dotnet/api/system.string.substring?view=net-5.0
- Erläuterungen zum 'split'-Operator in PowerShell: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.1
- Microsoft Dokumentation zu PowerShell: https://docs.microsoft.com/de-de/powershell/