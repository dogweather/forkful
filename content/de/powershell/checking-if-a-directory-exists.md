---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "C++: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein Prozess, bei dem festgestellt wird, ob ein spezifisches Datei- oder Verzeichnispfad vorhanden ist oder nicht. Programmierer tun dies, um Fehler zu vermeiden, die auftreten können, wenn ein nicht existierendes Verzeichnis verwendet wird.

## So Geht's:

Verwende das Cmdlet 'Test-Path' in PowerShell, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Übergeben Sie den Pfad des zu überprüfenden Verzeichnisses als Parameter. Hier ist ein einfaches Beispiel:

```PowerShell
$Pfad = 'C:\BeispielPfad'
if (Test-Path $Pfad)
{
    Write-Host "Verzeichnis existiert."
}
else
{
    Write-Host "Verzeichnis existiert nicht."
}
```
Wenn das Verzeichnis existiert, gibt das Skript "Verzeichnis existiert." aus. Ansonsten "Verzeichnis existiert nicht."

## Vertiefter Einblick

Historisch hat PowerShell immer das Cmdlet 'Test-Path' zur Überprüfung eines Verzeichnisses verwendet. Es ist effizient und verlässlich.

In PowerShell können Sie aber auch try-catch Blöcke verwenden, indem Sie versuchen, auf das Verzeichnis zuzugreifen und Fehler abzufangen. Dieser Ansatz ist jedoch weniger effizient als die Verwendung von 'Test-Path'.

Darüber hinaus implementiert Test-Path eine Überprüfung anhand von Wildcards. Also, wenn Sie etwas wie 'Test-Path C:\Beispiel*' verwenden, gibt er true zurück, wenn irgendetwas mit 'Beispiel' beginnt und existiert.
  
## Siehe Auch

Test-Path Cmdlet: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1

PowerShell Verzeichnisse und Dateien: http://www.itnator.net/programmieren/windows-powershell-verzeichnisse-und-dateien-verwalten/ 

PowerShell Fehlerbehandlung: https://www.it-administrator.de/themen/sicherheit/fachartikel/282906.html

Unterschiedliche Wege, Verzeichnisse zu überprüfen: https://stackoverflow.com/questions/29221072/different-ways-to-check-whether-a-directory-exists-using-powershell