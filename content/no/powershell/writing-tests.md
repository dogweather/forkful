---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester betyr å lage scripts som sjekker at kode fungerer som forventet. Programmere gjør dette for å sikre kvalitet og forebygge feil.

## How to:
For å skrive tester i PowerShell kan Pester, et rammeverk for testing, brukes. Her er et eksempel på en enkel test:

```PowerShell
# Installer Pester hvis det ikke allerede er installert
Install-Module -Name Pester -Force -SkipPublisherCheck

# Importer Pester-modulet
Import-Module Pester

# Skriv en enkel funksjon
function Get-MultiplyResult($x, $y) {
    return $x * $y
}

# Definer test
Describe "Get-MultiplyResult Tests" {
    It "multiplies two numbers" {
        Get-MultiplyResult -x 2 -y 3 | Should -Be 6
    }
}

# Kjør testen
Invoke-Pester
```

Forventet output:

```
Starting discovery in 1 files.
Discovering in C:\path\to\your\tests.tests.ps1.
Found 1 tests. 143ms
Discovery finished in 191ms.

Running tests from 'C:\path\to\your\tests.tests.ps1'
Describing Get-MultiplyResult Tests
 [+] multiplies two numbers 82ms (77ms|5ms)
Tests completed in 273ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Deep Dive
Pester ble introdusert i 2014 og ble raskt standard for PowerShell testing, delvis fordi det følger med i Windows 10 og nyere. Alternativer inkluderer PSUnit og PowerShell xUnit, men Pester er mest brukt. Pester tillater mocking, testdekning og BDD-stil testing og følger PowerShell skriptkonvensjoner, noe som gjør det til en naturlig del av PowerShell-utviklers verktøykasse.

## See Also
- [Pester, PowerShell testing framework](https://pester.dev/)
- [PowerShell Testing Integration med Visual Studio Code](https://code.visualstudio.com/docs/languages/powershell#_testing-powershell)
