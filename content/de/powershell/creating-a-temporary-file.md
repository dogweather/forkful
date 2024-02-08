---
title:                "Erstellung einer temporären Datei"
aliases:
- de/powershell/creating-a-temporary-file.md
date:                  2024-01-20T17:41:35.902827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Temporäre Dateien sind flüchtige Dateien, die während der Programmausführung genutzt werden. Programmierer schaffen sie für kurzlebige Aufgaben wie Zwischenspeicherung, Datenverarbeitung oder zum Testen.

## How to:
```PowerShell
# Eine temporäre Datei erstellen
$tempFile = [System.IO.Path]::GetTempFileName()

# In die temporäre Datei schreiben
Set-Content -Path $tempFile -Value 'Hier stehen meine temporären Daten'

# Inhalt der temporären Datei anzeigen
Get-Content -Path $tempFile

# Temporäre Datei löschen
Remove-Item -Path $tempFile
```
Beispiel-Ausgabe:
```
Hier stehen meine temporären Daten
```

## Deep Dive
Bevor `System.IO.Path]::GetTempFileName()` in .NET kam, erstellten Entwickler oft manuell temporäre Dateien, indem sie zufällige Namen generierten und Konflikte handhabten. Dieser Ansatz war fehleranfällig.

Alternative Wege, temporäre Dateien zu erstellen, sind `$env:TEMP` oder `$env:TMP` zu nutzen, um manuell Dateipfade im temporären Verzeichnis zu erstellen. Für mehr Kontrolle kann man das `[System.IO.Path]`-Objekt nutzen, um einen einzigartigen Dateinamen zu erstellen:

```PowerShell
$tempFolderPath = [System.IO.Path]::GetTempPath()
$uniqueFileName = [System.IO.Path]::GetRandomFileName()
$tempFilePath = Join-Path $tempFolderPath $uniqueFileName
```

Die Verwendung von `[System.IO.Path]::GetTempFileName()` ist aber sicherer und bequemer, da es automatisch einen einzigartigen Dateinamen generiert und die Datei im temporären Verzeichnis des Systems erstellt.

## See Also
- Microsoft Dokumentation über die `Path`-Klasse: https://docs.microsoft.com/dotnet/api/system.io.path
- PowerShell Dokumentation für `Get-Content`: https://docs.microsoft.com/powershell/module/microsoft.powershell.management/get-content
- PowerShell Dokumentation für `Set-Content`: https://docs.microsoft.com/powershell/module/microsoft.powershell.management/set-content
- PowerShell Dokumentation für `Remove-Item`: https://docs.microsoft.com/powershell/module/microsoft.powershell.management/remove-item
