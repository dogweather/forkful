---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:51.492693-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Długość łańcucha znaków to ilość znaków, z których się składa. Programiści muszą to sprawdzać, aby zarządzać tekstem, np. w walidacji, ekstrakcji danych, czy manipulacji ciągami.

## Jak to zrobić:
W PowerShell mierzenie długości łańcucha jest proste. Użyj właściwości `Length`

```PowerShell
$ciagZnakow = "Witaj, świecie!"
$dlugosc = $ciagZnakow.Length
$dlugosc
```
Wynik:
```
15
```
Proste, prawda?

## W Głąb Tematu
Historia: PowerShell wprowadzono w 2006 roku, zastępując starsze narzędzia skryptowe. Właściwość `Length` znajdziesz w wielu językach, bo pomiar długości łańcucha jest uniwersalny.

Alternatywy: Można też użyć metody `.ToCharArray()` i policzyć elementy, ale to już przesada.

Implementacja: PowerShell, jako nakładka na .NET, używa właściwości `Length` z klasy `String`. Czyli pod spodem to samo, co w C#.

## Zobacz Również
- [Dokumentacja Microsoft dla klasy String](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
- [PowerShell About Properties](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_properties?view=powershell-7.1)
