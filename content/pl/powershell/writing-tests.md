---
title:                "Pisanie testów"
date:                  2024-01-19
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to sprawdzanie kodu pod kątem błędów i niezamierzonych zachowań. Programiści robią to, by upewnić się, że ich kod jest solidny i niezawodny przed wdrożeniem.

## Jak to zrobić:
```PowerShell
# Instalacja modułu Pester - frameworka testowego PowerShell
Install-Module -Name Pester -Force -SkipPublisherCheck

# Przykładowy test sprawdzający działanie funkcji 'Add-Numbers'
function Add-Numbers ($a, $b) {
  return $a + $b
}

Describe "Add-Numbers Tests" {
  It "adds two numbers" {
    Add-Numbers 2 3 | Should -Be 5
  }
  
  It "fails for non-numeric input" {
    { Add-Numbers 2 'x' } | Should -Throw
  }
}

# Wykonanie testów
Invoke-Pester
```

Wyjście:
```
Describing Add-Numbers Tests
  [+] adds two numbers 1ms
  [+] fails for non-numeric input 76ms
```

## Zanurzamy się głębiej:
Pester, najpopularniejszy framework testowy w PowerShell, pojawił się w wersji 3.0 w 2015 roku, oferując bogatą składnię i wsparcie dla testowania infrastruktury. Alternatywy dla Pester to np. .NET-owe NUnit z dodatkowym modułem PSate. Ważne, że Pester pozwala na testowanie kodu jednostkowo (unit testing) i behawioralnie (behaviour-driven development - BDD).

## Zobacz również:
- [Pester, the PowerShell testing framework](https://pester.dev)
- [PowerShell Gallery | Pester](https://www.powershellgallery.com/packages/Pester)
