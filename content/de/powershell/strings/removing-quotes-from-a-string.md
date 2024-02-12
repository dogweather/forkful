---
title:                "Anführungszeichen aus einem String entfernen"
date:                  2024-01-26T03:41:26.273726-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String in PowerShell entfernt einzelne (`'`) oder doppelte (`"`) Anführungszeichen, die Ihren Text umgeben. Programmierer müssen oft Strings für die Verarbeitung, den Vergleich oder die Ausgabe bereinigen, insbesondere wenn sie mit Benutzereingaben oder Dateiparsing zu tun haben.

## Wie:
Sie können den Operator `-replace` verwenden, um Anführungszeichen aus einem String zu entfernen. So geht's:

```PowerShell
# Einzelne Anführungszeichen ersetzen
$stringWithSingleQuotes = "'Hallo, Welt!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Ausgabe: Hallo, Welt!

# Doppelte Anführungszeichen ersetzen
$stringWithDoubleQuotes = '"Hallo, Welt!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Ausgabe: Hallo, Welt!
```

Für beide Typen:

```PowerShell
$stringWithQuotes = '"Hallo dort," sagte sie.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Beachten Sie die Verwendung der Regex-Zeichenklasse
Write-Output $cleanString  # Ausgabe: Hallo dort, sagte sie.
```

Beispielausgaben aus der Konsole sehen ungefähr so aus:

```
Hallo, Welt!
Hallo, Welt!
Hallo dort, sagte sie.
```

## Vertiefung
In den Tagen, bevor PowerShell ein Funkeln in Microsofts Auge war, war die Textverarbeitung in Windows oft das Reich von Batch-Skripten mit begrenzten Fähigkeiten. Die Einführung von PowerShell brachte leistungsfähige Zeichenketten-Manipulationsfunktionen mit sich, die das Scripting viel robuster machten.

Alternativen zu `-replace` existieren, wie die Verwendung der `.Trim()`-Methode, um Anführungszeichen nur am Anfang und am Ende eines Strings zu entfernen, aber sie bieten nicht dieselbe Kontrolle oder Regex-Unterstützung.

```PowerShell
# Verwenden von .Trim() für Anführungszeichen am Anfang und Ende
$stringWithQuotes = '"Hallo, Welt!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Ausgabe: Hallo, Welt!
```

Beachten Sie, dass `-replace` hinter den Kulissen Regex verwendet, also wenn Sie damit arbeiten, denken Sie daran, dass spezielle Zeichen escaped werden müssen, wenn Sie sie gezielt entfernen möchten. Wenn Sie eine genauere Kontrolle über das Entfernen von Anführungszeichen benötigen, ist das Eintauchen in Regex mit `-replace` der Weg, der Ihnen enorme Flexibilität bietet.

## Siehe auch
- Für mehr über Regex in PowerShell, schauen Sie in die offiziellen Dokumentationen: [about_Regular_Expressions](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Entdecken Sie andere Zeichenkettenmethoden: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/de-de/dotnet/api/system.string.trim?view=net-6.0)