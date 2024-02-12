---
title:                "Scrivere test"
date:                  2024-02-03T19:31:34.408779-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test in PowerShell comporta la creazione di script che validano automaticamente la funzionalità del vostro codice PowerShell, garantendo che si comporti come previsto. I programmatori fanno questo per catturare gli errori precocemente, semplificare la manutenzione del codice e assicurare che le modifiche al codice non rompano involontariamente le funzionalità esistenti.

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
