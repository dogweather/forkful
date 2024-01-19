---
title:                "Zamiana liter w ciągu na wielkie"
html_title:           "PowerShell: Zamiana liter w ciągu na wielkie"
simple_title:         "Zamiana liter w ciągu na wielkie"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana stringów na wielkie litery oznacza, że zamieniamy wszystkie małe litery znaków w ciągu na duże litery. Programiści często robią to, żeby zachować spójność danych wejściowych lub ułatwić porównywanie stringów.

## Jak to zrobić:

W PowerShell, możesz to zrobić używając metody `.ToUpper()` na stringu. 

```PowerShell
$string = "cześć, jak się masz?"
$capitalizedString = $string.ToUpper()

Write-Output $capitalizedString
```

Wynik:

```PowerShell
CZEŚĆ, JAK SIĘ MASZ?
```

## Głębsze spojrzenie

Historycznie, konwersja stringów na wielkie litery była używana, aby ułatwić porównywanie stringów, ponieważ `ABC` jest traktowane tak samo jak `abc` gdy przejdzie przez funkcję `.ToUpper()`. 

W PowerShell, inna metoda do porównywania stringów bez sensu na wielkość liter to używanie `-ieq` operatora. Przykład:

```PowerShell
$string1 = "cześć"
$string2 = "CZEŚĆ"

if ($string1 -ieq $string2) {
    Write-Output "Stringi są takie same"
} else {
    Write-Output "Stringi są różne"
}
```

Wynik:

```PowerShell
Stringi są takie same
```

Chociaż `.ToUpper()` jest proste i łatwe do zrozumienia, operator `-ieq` jest bardziej wydajny pod względem zasobów, jeśli tylko chcemy porównać stringi.

## Zobacz też

[A Guide to Strings in PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-data-basics-part-1/)
[String Methods in PowerShell](https://www.urtech.ca/2017/08/solved-powershell-string-manipulation/)
[String Comparison in PowerShell](https://ss64.com/ps/syntax-compare.html)