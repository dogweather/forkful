---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:34.408779-07:00
description: "Scrivere test in PowerShell comporta la creazione di script che validano\
  \ automaticamente la funzionalit\xE0 del vostro codice PowerShell, garantendo che\
  \ si\u2026"
lastmod: '2024-03-13T22:44:43.646129-06:00'
model: gpt-4-0125-preview
summary: "Scrivere test in PowerShell comporta la creazione di script che validano\
  \ automaticamente la funzionalit\xE0 del vostro codice PowerShell, garantendo che\
  \ si comporti come previsto."
title: Scrivere test
weight: 36
---

## Come fare:
PowerShell non ha un framework di test incorporato, ma Pester, un modulo di terze parti popolare, è ampiamente usato per scrivere ed eseguire test. Ecco come iniziare con Pester per testare le vostre funzioni PowerShell.

Prima, installare Pester se non lo avete già fatto:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Successivamente, supponete di avere una semplice funzione PowerShell che volete testare, salvata come `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Per testare questa funzione con Pester, create uno script di test nominato `MyFunction.Tests.ps1`. In questo script, usate i blocchi `Describe` e `It` di Pester per definire i casi di test:

```powershell
# Importare la funzione da testare
. .\MyFunction.ps1

Describe "Test di Get-MultipliedNumber" {
    It "Moltiplica il numero per 2 quando non è fornito un moltiplicatore" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Moltiplica correttamente il numero con il moltiplicatore fornito" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Per eseguire i test, aprire PowerShell, navigare nella directory che contiene lo script di test e usare il comando `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

L'output di esempio apparirà così, indicando se i vostri test sono passati o falliti:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\percorso\a\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0, NotRun: 0
```

Questo output mostra che entrambi i test sono passati, dandovi la fiducia che la vostra funzione `Get-MultipliedNumber` si comporti come previsto negli scenari che avete testato.
