---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:45.511524-07:00
description: "Hvordan: PowerShell har ikke et innebygd testrammeverk, men Pester,\
  \ en popul\xE6r tredjepartsmodul, brukes mye til \xE5 skrive og kj\xF8re tester.\
  \ Her er hvordan\u2026"
lastmod: '2024-03-13T22:44:41.020972-06:00'
model: gpt-4-0125-preview
summary: "PowerShell har ikke et innebygd testrammeverk, men Pester, en popul\xE6\
  r tredjepartsmodul, brukes mye til \xE5 skrive og kj\xF8re tester."
title: Skrive tester
weight: 36
---

## Hvordan:
PowerShell har ikke et innebygd testrammeverk, men Pester, en populær tredjepartsmodul, brukes mye til å skrive og kjøre tester. Her er hvordan du kommer i gang med Pester for å teste dine PowerShell-funksjoner.

Først, installer Pester hvis du ikke allerede har gjort det:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Deretter, anta at du har en enkel PowerShell-funksjon du vil teste, lagret som `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

For å teste denne funksjonen med Pester, opprett et testsikript med navnet `MyFunction.Tests.ps1`. I dette skriptet, bruk Pesters `Describe` og `It` blokker for å definere testtilfellene:

```powershell
# Importer funksjonen som skal testes
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tester" {
    It "Multipliserer tall med 2 når ingen multiplikator er oppgitt" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multipliserer korrekt tall med gitt multiplikator" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

For å kjøre testene, åpne PowerShell, naviger til katalogen som inneholder testskriptet ditt, og bruk kommandoen `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Eksempelutdata vil se slik ut, og indikerer om testene dine besto eller feilet:

```
Starter oppdagelse i 1 filer.
Oppdagelse fullført på 152ms.
[+] C:\sti\til\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tester fullført på 204ms
Tester Bestått: 2, Feilet: 0, Hoppet over: 0 IkkeKjørt: 0
```

Denne utdataen viser at begge testene ble bestått, noe som gir deg tillit til at din `Get-MultipliedNumber` funksjon oppfører seg som forventet under scenariene du har testet.
