---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Erstellen einer temporären Datei ist das Erzeugen einer Datei, die zur kurzfristigen Lagerung von Daten dient, meistens für den Bereich eines einzelnen Laufausdrucks bzw. einer Programmsitzung. Programmierer machen dies, wenn sie Daten zwischenspeichern oder temporäre Arbeitsspeicher-Objekte erzeugen müssen, die groß genug sind, dass die Daten nicht einfach in eine Variable geladen werden können.

## So geht's:

Erstell eine temporäre Datei mit PowerShell ist nur eine Zeile Code. Die `New-TemporaryFile` cmdlet erstellt eine temporäre Datei und gibt den vollständigen Pfad zur Datei zurück.

Erstellen die temp-Datei:
```PowerShell
$tempFile = New-TemporaryFile
```
Sie können den vollständigen Pfad zur Datei anzeigen:
```PowerShell
Write-Output $tempFile.FullName
```
Dies führt zu einer Ausgabe ähnlich wie:
```PowerShell
C:\Users\YourUser\AppData\Local\Temp\tmp1BF0.tmp
```
Und nachdem Sie fertig sind, können Sie die Datei löschen:
```PowerShell
Remove-Item -Path $tempFile.FullName
```

## Vertiefung:

Das Konzept temporärer Dateien stammt aus den alten Zeiten, als Computer noch nicht über ausreichend RAM verfügten und man große Datenmengen auf die Festplatte auslagern musste.

Es gibt alternative Möglichkeiten, temporäre Dateien zu erstellen; Sie können beispielsweise die .NET-Klasse `[System.IO.Path]::GetTempFileName()` verwenden, die im Grunde dasselbe tut, aber etwas flexibler ist.

Beim Umgang mit temporären Dateien sollten Sie immer sicherstellen, dass Sie diese löschen, nachdem Sie mit ihnen fertig sind. Einer der Gründe, warum es den `New-TemporaryFile` cmdlet gibt, ist die Notwendigkeit, diesen Aspekt der Ressourcenverwaltung auf niedriger Ebene zu handhaben.

## Siehe Auch:

1. [New-TemporaryFile auf Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7.1)
2. [Remove-Item auf Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/remove-item?view=powershell-7.1)
3. [System.IO.Path::GetTempFileName auf Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)