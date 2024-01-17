---
title:                "Ein temporäres Datei erstellen"
html_title:           "PowerShell: Ein temporäres Datei erstellen"
simple_title:         "Ein temporäres Datei erstellen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Creating a temporary file ist eine nützliche Technik, die Programmierer anwenden, um temporäre Dateien zu erstellen, die während der Ausführung eines Skripts oder Programms verwendet werden. Dies ermöglicht es ihnen, temporäre Daten effizient zu speichern und zu verwalten und sie später wieder zu löschen.

## How to:
Um eine temporäre Datei in PowerShell zu erstellen, kannst du das `New-TemporaryFile` Cmdlet verwenden, das Teil des `PowerShellGet` Moduls ist. Hier ist ein Beispiel, das eine temporäre Textdatei erstellt und ihren Inhalt ausgibt:

```PowerShell
$tempFile = New-TemporaryFile -Suffix ".txt"
Set-Content $tempFile "Das ist eine temporäre Datei."
Get-Content $tempFile
```

Dies könnte die folgende Ausgabe erzeugen:

```PowerShell
Das ist eine temporäre Datei.
```

## Deep Dive:
Die Verwendung von temporären Dateien ist keine neue Technik und wird seit langem von Programmierern in verschiedenen Sprachen und Umgebungen verwendet. Früher mussten Programmierer manuell temporäre Dateien erstellen und verwalten, was zeitaufwendig und fehleranfällig sein konnte. Mit der Einführung von speziellen Temporärdatei-Funktionalitäten, wie dem `New-TemporaryFile` Cmdlet in PowerShell, wird die Erstellung und Verwaltung dieser Dateien deutlich vereinfacht.

Als Alternative zur Verwendung von `New-TemporaryFile` können Programmierer auch `System.IO.Path.GetTempFileName()` verwenden, um eine temporäre Datei in C# zu erstellen. Dies erzeugt eine Datei-Plattform-unabhängige temporäre Datei.

Bei der Implementierung von `New-TemporaryFile` in PowerShell wird standardmäßig der Ordner `%TEMP%` als Speicherort für die Datei verwendet. Programmierer können jedoch auch einen anderen Speicherort durch Angabe des Parameters `-Path` auswählen.

## Siehe auch:
Weitere Informationen über das `New-TemporaryFile` Cmdlet und das PowerShellGet Modul findest du in der offiziellen Microsoft-Dokumentation unter [docs.microsoft.com/en-us/powershell/module/powershellget/new-temporaryfile?view=powershell-7](https://docs.microsoft.com/en-us/powershell/module/powershellget/new-temporaryfile?view=powershell-7).

Für C#-Programmierer gibt es weitere Möglichkeiten zur Erstellung temporärer Dateien, die in der offiziellen Microsoft-Dokumentation zu `Path.GetTempFileName()` beschrieben sind: [docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0).