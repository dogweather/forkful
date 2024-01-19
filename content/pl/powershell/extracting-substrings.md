---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnianie podciągów (substringów) polega na uzyskiwaniu mniejszych ciągów z większego ciągu. Programiści robią to, aby manipulować tylko częścią danych, zamiast całości.

## Jak to zrobić:

W PowerShell, aby wyodrębnić podciąg, używa się metody `.substring()`. Oto przykład:

```PowerShell
$str = "To jest łańcuch znaków"
# Wyciągnij podciąg zaczynając od indeksu 2 długości 6
$subStr = $str.substring(2,6) 
echo $subStr
```

Output:

```PowerShell
jest ła
```

## Głębsza wiedza:
* **Kontekst historyczny**: Metoda substring pojawiła się po raz pierwszy w języku programowania C, a następnie została przeniesiona do innych języków, włączając PowerShell.
* **Alternatywy**: Inne metody manipulacji ciągami obejmują `split`, `replace` i `trim`. Wybór zależy od konkretnego zadania.
* **Szczegóły implementacji**: Wyodrębnianie podciągów w PowerShell jest zero-based. Pierwszy indeks to 0. Jeżeli długość nie jest podana, metoda substring zwraca wszystko do końca ciągu.

## Zobacz też:

* Dokumentacja Microsoft dla [`substring`](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0) w .NET 5.0.
* Dokumentacja Microsoft dla [manipulacji ciągami](https://docs.microsoft.com/pl-pl/powershell/scripting/samples/string-manipulation?view=powershell-7.1) w PowerShell.