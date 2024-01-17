---
title:                "Porównywanie dwóch dat"
html_title:           "PowerShell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Porównywanie dwóch dat to porównywanie dat względem siebie w celu ustalenia, która jest wcześniejsza, późniejsza lub czy są identyczne. Programiści często to robią, ponieważ jest to prosta i szybka metoda, aby sprawdzić poprawność funkcji obsługujących daty w skrypcie.

## Jak to zrobić:
```PowerShell
# Ustaw dwie zmienne z datami
$Data1 = Get-Date -Year 2021 -Month 1 -Day 1 
$Data2 = Get-Date 

# Porównaj daty
Compare-Object -ReferenceObject $Data1 -DifferenceObject $Data2 

# Przykładowy wynik: wyświetla, czy Data1 jest wcześniejsza (<=) czy późniejsza (>=) od Data2 
<=>$Data1 <= $Data2 
```

## Głębszy zanurz
Porównywanie dat jest ważne, ponieważ wiele programów i aplikacji musi prawidłowo obsługiwać daty. W przeszłości jedyną dostępną metodą było ręczne porównywanie dat za pomocą funkcji warunkowych (if/else). Alternatywą w PowerShell jest użycie operatora porównania (-eq, -gt, itp.), który zwraca wartość logiczną True lub False w zależności od wyniku porównania. Implementacja komendy Compare-Object została wprowadzona w wersji PowerShell v1.0.

## Zobacz też
- [Dokumentacja operatorów porównania w PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7)
- [Article about how to compare two dates in PowerShell by Devblogs](https://devblogs.microsoft.com/scripting/use-powershell-to-compare-two-dates/)