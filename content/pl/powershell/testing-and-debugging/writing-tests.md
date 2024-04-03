---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:44.241871-07:00
description: "Pisanie test\xF3w w PowerShellu polega na tworzeniu skrypt\xF3w, kt\xF3\
  re automatycznie weryfikuj\u0105 funkcjonalno\u015B\u0107 Twojego kodu PowerShell,\
  \ zapewniaj\u0105c, \u017Ce\u2026"
lastmod: '2024-03-13T22:44:35.633463-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w PowerShellu polega na tworzeniu skrypt\xF3w, kt\xF3\
  re automatycznie weryfikuj\u0105 funkcjonalno\u015B\u0107 Twojego kodu PowerShell,\
  \ zapewniaj\u0105c, \u017Ce zachowuje si\u0119 on zgodnie z oczekiwaniami."
title: "Pisanie test\xF3w"
weight: 36
---

## Co i dlaczego?

Pisanie testów w PowerShellu polega na tworzeniu skryptów, które automatycznie weryfikują funkcjonalność Twojego kodu PowerShell, zapewniając, że zachowuje się on zgodnie z oczekiwaniami. Programiści robią to, aby wyłapywać błędy na wczesnym etapie, upraszczać konserwację kodu i zapewniać, że modyfikacje kodu nie zakłócają niezamierzenie istniejącej funkcjonalności.

## Jak to zrobić:

PowerShell nie ma wbudowanego frameworka do testowania, ale szeroko stosowanym, popularnym modułem zewnętrznym do pisania i uruchamiania testów jest Pester. Oto, jak zacząć używać Pestera do testowania twoich funkcji PowerShell.

Najpierw zainstaluj Pester, jeśli jeszcze tego nie zrobiłeś:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Następnie, zakładając, że masz prostą funkcję PowerShell, którą chcesz przetestować, zapisaną jako `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Aby przetestować tę funkcję za pomocą Pestera, utwórz skrypt testowy o nazwie `MyFunction.Tests.ps1`. W tym skrypcie użyj bloków `Describe` i `It` Pestera, aby zdefiniować przypadki testowe:

```powershell
# Importuj funkcję do testowania
. .\MyFunction.ps1

Describe "Testy Get-MultipliedNumber" {
    It "Mnoży liczbę przez 2, gdy nie podano mnożnika" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Poprawnie mnoży liczbę przez podany mnożnik" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Aby uruchomić testy, otwórz PowerShell, przejdź do katalogu zawierającego skrypt testowy i użyj polecenia `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Przykładowy wynik będzie wyglądał tak, wskazując, czy twoje testy zakończyły się sukcesem, czy porażką:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\ścieżka\do\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

Ten wynik pokazuje, że oba testy zakończyły się sukcesem, dając ci pewność, że twoja funkcja `Get-MultipliedNumber` zachowuje się zgodnie z oczekiwaniami w scenariuszach, które przetestowałeś.
