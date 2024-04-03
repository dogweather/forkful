---
date: 2024-01-20 17:56:38.076474-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.645407-06:00'
model: gpt-4-1106-preview
summary: .
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## Jak to zrobić:
```PowerShell
# Przykład skryptu 'hello.ps1', który przyjmuje argumenty

param(
    [string]$name,
    [int]$repeat = 1
)

for ($i=0; $i -lt $repeat; $i++) {
    Write-Output "Cześć $name!"
}

# Wywołanie skryptu z argumentami
PS > .\hello.ps1 -name "Świat" -repeat 3

# Output:
Cześć Świat!
Cześć Świat!
Cześć Świat!
```

## Deep Dive
W przeszłości niewielu programistów PowerShell korzystało z argumentów wiersza poleceń, ale od kiedy skrypty stały się bardziej zaawansowane, stało się to bardziej powszechne. Alternatywą były stałe wartości kodowane bezpośrednio w skrypcie lub interaktywne pytania do użytkownika. Dziś korzystanie z `param` dla określenia wejściowych parametrów skryptu jest standardową praktyką. Dodatkowo, `$args` pozwala na elastyczne przetwarzanie, kiedy niewymagana jest jasno zdefiniowana struktura parametrów.

## See Also
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
- [PowerShell Docs](https://docs.microsoft.com/en-us/powershell/)
