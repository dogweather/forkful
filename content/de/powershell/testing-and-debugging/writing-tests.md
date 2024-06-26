---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.495071-07:00
description: "Wie: PowerShell verf\xFCgt nicht \xFCber ein eingebautes Testframework,\
  \ aber Pester, ein beliebtes Drittanbieter-Modul, wird weitgehend verwendet, um\
  \ Tests zu\u2026"
lastmod: '2024-03-13T22:44:54.107516-06:00'
model: gpt-4-0125-preview
summary: "PowerShell verf\xFCgt nicht \xFCber ein eingebautes Testframework, aber\
  \ Pester, ein beliebtes Drittanbieter-Modul, wird weitgehend verwendet, um Tests\
  \ zu schreiben und auszuf\xFChren."
title: Tests Schreiben
weight: 36
---

## Wie:
PowerShell verfügt nicht über ein eingebautes Testframework, aber Pester, ein beliebtes Drittanbieter-Modul, wird weitgehend verwendet, um Tests zu schreiben und auszuführen. Hier erfahren Sie, wie Sie mit Pester beginnen können, um Ihre PowerShell-Funktionen zu testen.

Zuerst installieren Sie Pester, falls Sie das noch nicht getan haben:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Betrachten Sie als nächstes eine einfache PowerShell-Funktion, die Sie testen möchten, gespeichert als `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Um diese Funktion mit Pester zu testen, erstellen Sie ein Testsript mit dem Namen `MyFunction.Tests.ps1`. In diesem Skript verwenden Sie Pestersiste `Describe`- und `It`-Blöcke, um die Testfälle zu definieren:

```powershell
# Importiere die zu testende Funktion
. .\MyFunction.ps1

Describe "Get-MultipliedNumber Tests" {
    It "Multipliziert Zahl mit 2, wenn kein Multiplikator angegeben wird" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multipliziert Zahl korrekt mit gegebenem Multiplikator" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Um die Tests auszuführen, öffnen Sie PowerShell, navigieren zum Verzeichnis, das Ihr Testskript enthält, und verwenden den Befehl `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Eine beispielhafte Ausgabe sieht folgendermaßen aus und zeigt an, ob Ihre Tests bestanden oder fehlgeschlagen sind:

```
Starte Entdeckung in 1 Dateien.
Entdeckung abgeschlossen in 152ms.
[+] C:\Pfad\zu\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests abgeschlossen in 204ms
Tests Bestanden: 2, Fehlgeschlagen: 0, Übersprungen: 0 NichtGelaufen: 0
```

Diese Ausgabe zeigt, dass beide Tests bestanden haben und gibt Ihnen die Sicherheit, dass Ihre `Get-MultipliedNumber`-Funktion unter den getesteten Szenarien wie erwartet funktioniert.
