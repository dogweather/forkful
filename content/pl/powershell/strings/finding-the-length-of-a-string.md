---
date: 2024-01-20 17:47:51.492693-07:00
description: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3w to ilo\u015B\u0107\
  \ znak\xF3w, z kt\xF3rych si\u0119 sk\u0142ada. Programi\u015Bci musz\u0105 to sprawdza\u0107\
  , aby zarz\u0105dza\u0107 tekstem, np. w walidacji, ekstrakcji danych,\u2026"
lastmod: '2024-02-25T18:49:33.987405-07:00'
model: gpt-4-1106-preview
summary: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3w to ilo\u015B\u0107 znak\xF3\
  w, z kt\xF3rych si\u0119 sk\u0142ada. Programi\u015Bci musz\u0105 to sprawdza\u0107\
  , aby zarz\u0105dza\u0107 tekstem, np. w walidacji, ekstrakcji danych,\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
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
