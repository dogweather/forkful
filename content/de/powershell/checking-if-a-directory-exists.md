---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "PowerShell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Bestandteil der Programmierung mit PowerShell. Es ermöglicht uns, zu prüfen, ob ein bestimmtes Verzeichnis auf unserer Festplatte vorhanden ist und gegebenenfalls darauf basierend unsere Skripte anzupassen.

## Wie funktioniert es?
Um zu überprüfen, ob ein Verzeichnis existiert, müssen wir lediglich das Cmdlet `Test-Path` verwenden. Ein Beispiel dafür sieht folgendermaßen aus:

```PowerShell
Test-Path C:\Users\Username\Documents
```

Wenn das Verzeichnis existiert, gibt PowerShell `True` zurück. Ansonsten wird `False` zurückgegeben.

## Tiefgehende Einblicke
Das Überprüfen von Verzeichnissen existiert nicht erst seit PowerShell. Schon frühere Skriptsprachen, wie zum Beispiel Bash, boten ähnliche Funktionen an. Alternativ zu `Test-Path` gibt es auch das Cmdlet `Get-ChildItem`, welches ebenfalls verwendet werden kann, um Verzeichnisse zu prüfen.

## Siehe auch
- [Microsoft Dokumentation zu Test-Path](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)
- [Get-ChildItem Cmdlet Dokumentation](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7)