---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests im Programmierkontext automatisieren den Überprüfungsprozess von Code, um sicherzustellen, dass er wie erwartet funktioniert. Programmierer schreiben Tests, um Fehler frühzeitig zu erkennen, die Qualität des Codes zu verbessern und das Vertrauen in ihre Software zu stärken.

## How to:
PowerShell nutzt das `Pester`-Framework für Tests. Mit `Describe`, `Context` und `It` baust du strukturierte Tests. Hier ein einfaches Beispiel:

```PowerShell
# Installiere Pester, falls noch nicht vorhanden
# Install-Module -Name Pester -Force -SkipPublisherCheck

# Importiere Pester
Import-Module Pester

# Schreibe einen Test
Describe "Testen meiner Funktion" {
    Context "Wenn meine Funktion mit gültigen Parametern aufgerufen wird" {
        It "gibt 'Hallo Welt!' zurück" {
            # Eventuelle Funktion, die getestet wird
            function SendeBegrüßung() {
                return "Hallo Welt!"
            }
            SendeBegrüßung | Should -Be "Hallo Welt!"
        }
    }
}

# Führe den Test aus
Invoke-Pester
```

Ausgabe könnte so aussehen:

```shell
Executing all tests in '.'

Executing script /Path/To/Test.ps1

  Describing Testen meiner Funktion
    Context Wenn meine Funktion mit gültigen Parametern aufgerufen wird
      [+] gibt 'Hallo Welt!' zurück 40ms
Tests completed in 40ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Deep Dive
Pester wurde 2014 als das führende Test-Framework für PowerShell eingeführt. Alternativ gibt es Tools wie `psake`, `Invoke-Build` für Build-Automatisierung und `PlatyPS` für die Dokumentation, aber für das reine Testing bleibt Pester unangefochten. Effektives Testen erfordert Verständnis vom Test-Scope, Mocking und Code Coverage.

## See Also
- Pester: https://pester.dev
- PowerShell Testing: https://docs.microsoft.com/en-us/powershell/scripting/dev-cross-plat/write-pester-tests?view=powershell-7.1
- PowerShell Gallery Pester-Seite: https://www.powershellgallery.com/packages/Pester
