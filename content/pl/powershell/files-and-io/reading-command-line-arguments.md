---
title:                "Odczytywanie argumentów linii poleceń"
aliases:
- /pl/powershell/reading-command-line-arguments/
date:                  2024-01-20T17:56:38.076474-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W PowerShellu argumenty wiersza poleceń to dane przekazywane do skryptu przy jego wywoływaniu. Programiści używają ich, by dostosować działanie skryptu bez modyfikowania jego kodu - to szybkie i elastyczne rozwiązanie.

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
