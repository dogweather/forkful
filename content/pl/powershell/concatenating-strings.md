---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Łączenie ciągów (string concatenation) to proste polegające na złączeniu dwóch lub więcej ciągów znaków (strings) w jeden. Programiści często to robią, aby tworzyć dynamiczne komunikaty, sformułowania czy ścieżki do plików.

## Jak to zrobić:

Połączenie ciągów w PowerShell jest łatwe. Najprostszym sposobem jest użycie operatora '+'. Dajmy na to:

```PowerShell
$str1 = "Cześć, "
$str2 = "świecie!"
$połączone = $str1 + $str2
Write-Output $połączone
```

Wyjście:

```PowerShell
Cześć, świecie!
```

Pamiętaj, że za pomocą operatora '+' możesz łączyć tylko ciągi. Jeśli chcesz dodać liczby do ciągu, musisz je najpierw przekonwertować na ciągi.

## Głębsze zrozumienie:

Historia łączenia ciągów jest stara jak programowanie. Jest to jedna z najprostszych operacji, które programista może wykonać.

W PowerShell, alternatywą dla operatora '+' jest użycie metody '.Concat()' klasy String:

```PowerShell
$str1 = "Cześć, "
$str2 = "świecie!"
$połączone = [string]::Concat($str1, $str2)
Write-Output $połączone
```

Wydanie będzie takie samo jak wcześniej.

Warto wspomnieć, że operator '+' tworzy nowy ciąg, a nie zmienia istniejących. To ważne w kontekście zarządzania pamięcią.

## Zobacz też:

1. Szczegółowy przewodnik po ciągach w PowerShell: https://ss64.com/ps/syntax-strings.html
2. Oficjalna dokumentacja Microsoftu na temat ciągów w PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1
3. Dokumentacja Microsoftu na temat operatorów w PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1