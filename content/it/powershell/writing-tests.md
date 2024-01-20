---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test nel codice è come assicurarsi che ogni funzione faccia quel che deve. I programmatori testano per evitare bug, risparmiare tempo e migliorare la qualità del software.

## Come fare:
Ecco come scrivere un semplice test in Pester, il framework di test per PowerShell:

```PowerShell
# Installa Pester
Install-Module -Name Pester -Force -SkipPublisherCheck

# Scrivi il test
Describe "Controllo della funzione Add-Numbers" {
    It "dovrebbe sommare due numeri" {
        $result = Add-Numbers -Number1 5 -Number2 3
        $result | Should -Be 8
    }
}

# Funzione da testare
function Add-Numbers {
    param (
        [Parameter(Mandatory=$true)]
        [int]$Number1,

        [Parameter(Mandatory=$true)]
        [int]$Number2
    )

    return $Number1 + $Number2
}

# Esegui il test
Invoke-Pester
```

Output esempio:

```
Describing Controllo della funzione Add-Numbers
 [+] dovrebbe sommare due numeri 82ms
Tests Completed: 1, Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Approfondimento
Pester è il principale framework di test per PowerShell, introdotto nel 2010. Alternative includono moduli come PSUnit e PowerShellTest. Pester si adatta perfettamente al TDD (Test-Driven Development) e si integra bene con CI/CD pipelines.

## Vedi anche
- Pester GitHub Repo: [https://github.com/pester/Pester](https://github.com/pester/Pester)
- Articolo sul TDD: [https://martinfowler.com/bliki/TestDrivenDevelopment.html](https://martinfowler.com/bliki/TestDrivenDevelopment.html)