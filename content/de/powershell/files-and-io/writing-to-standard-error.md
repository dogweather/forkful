---
title:                "Schreiben auf Standardfehler"
aliases: - /de/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:07.028747-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf den Standardfehler (stderr) in PowerShell bedeutet, Fehlermeldungen oder Diagnosen direkt an den stderr-Datenstrom zu senden, welcher sich vom Standardausgabe-Datenstrom (stdout) unterscheidet. Diese Trennung ermöglicht eine präzisere Kontrolle über die Ausgabe eines Skripts und ermöglicht es Entwicklern, normale und Fehlermeldungen an verschiedene Ziele zu leiten, was für die Fehlerbehandlung und Protokollierung grundlegend ist.

## Wie geht das:

PowerShell vereinfacht den Prozess des Schreibens auf stderr durch die Verwendung des `Write-Error` Cmdlets oder durch das Umleiten der Ausgabe an die Methode `$host.ui.WriteErrorLine()`. Für eine direkte stderr-Umleitung könnten Ihnen jedoch .NET-Methoden oder die von PowerShell selbst angebotene Dateideskriptor-Umleitung bevorzugt sein.

**Beispiel 1:** Verwendung von `Write-Error` um eine Fehlermeldung auf stderr zu schreiben.

```powershell
Write-Error "Dies ist eine Fehlermeldung."
```

Ausgabe auf stderr:
```
Write-Error: Dies ist eine Fehlermeldung.
```

**Beispiel 2:** Verwendung von `$host.ui.WriteErrorLine()` für direktes Schreiben auf stderr.

```powershell
$host.ui.WriteErrorLine("Direktes Schreiben auf stderr.")
```

Ausgabe auf stderr:
```
Direktes Schreiben auf stderr.
```

**Beispiel 3:** Verwendung von .NET-Methoden zum Schreiben auf stderr.

```powershell
[Console]::Error.WriteLine("Verwendung der .NET-Methode für stderr")
```

Ausgabe dieser Methode:
```
Verwendung der .NET-Methode für stderr
```

**Beispiel 4:** Umleitung der Fehlerausgabe mit dem Dateideskriptor `2>`.

Dateideskriptoren in PowerShell können verschiedene Datenströme umleiten. Für stderr ist der Dateideskriptor `2`. Hier ist ein Beispiel für die Umleitung von stderr in eine Datei namens `error.log` während der Ausführung eines Befehls, der einen Fehler erzeugt.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Dieses Beispiel erzeugt keine Konsolenausgabe, generiert aber eine Datei `error.log` im aktuellen Verzeichnis, die die Fehlermeldung enthält, die beim Versuch entsteht, auf eine nicht existierende Datei zuzugreifen.

Zusammenfassend bietet PowerShell mehrere Methoden zum effektiven Schreiben und Verwalten der Fehlerausgabe und ermöglicht somit komplexe Strategien zur Fehlerbehandlung und Protokollierung in Skripten und Anwendungen.
