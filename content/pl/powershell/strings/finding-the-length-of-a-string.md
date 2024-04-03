---
date: 2024-01-20 17:47:51.492693-07:00
description: "Jak to zrobi\u0107: W PowerShell mierzenie d\u0142ugo\u015Bci \u0142\
  a\u0144cucha jest proste. U\u017Cyj w\u0142a\u015Bciwo\u015Bci `Length`."
lastmod: '2024-03-13T22:44:35.618813-06:00'
model: gpt-4-1106-preview
summary: "W PowerShell mierzenie d\u0142ugo\u015Bci \u0142a\u0144cucha jest proste."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
