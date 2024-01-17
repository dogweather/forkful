---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "PowerShell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 
Znalezienie długości ciągu znaków jest ważnym elementem programowania, ponieważ pozwala na określenie ile znaków zawiera dany ciąg. Jest to przydatne zwłaszcza przy manipulowaniu tekstem, gdzie często musimy sprawdzić długość wyrazów lub zdań.

## Jak to zrobić: 
Aby znaleźć długość ciągu znaków w programie PowerShell, możemy użyć wbudowanej funkcji `Len`. Poniżej przedstawiamy przykładowy kod i jego output:

```
PowerShell
$zmienna = "Hello World!"  
Len $zmienna
```

Output: 
```
12
```

## Głębszy wgląd: 
Funkcja `Len` jest dostępna w wielu językach programowania i została wprowadzona już w pierwszych wersjach programu BASIC w latach 60-tych. W niektórych językach, np. Java, istnieje również metoda `length()`, która może być użyta do znalezienia długości ciągu znaków.

Alternatywą dla wbudowanej funkcji `Len` w programie PowerShell może być zastosowanie operatora `.` po zmiennej, a następnie metody `Length`, na przykład `$zmienna.Length`.

## Zobacz także: 
Jeśli chcesz dowiedzieć się więcej o innych funkcjach i zastosowaniach programu PowerShell, polecamy zajrzeć na oficjalną stronę dokumentacji: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)