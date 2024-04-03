---
date: 2024-01-26 03:41:13.742061-07:00
description: "Jak to zrobi\u0107: Mo\u017Cesz u\u017Cy\u0107 operatora `-replace`\
  \ do usuni\u0119cia cudzys\u0142ow\xF3w ze stringa. Oto jak."
lastmod: '2024-03-13T22:44:35.615764-06:00'
model: gpt-4-0125-preview
summary: "Mo\u017Cesz u\u017Cy\u0107 operatora `-replace` do usuni\u0119cia cudzys\u0142\
  ow\xF3w ze stringa."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Możesz użyć operatora `-replace` do usunięcia cudzysłowów ze stringa. Oto jak:

```PowerShell
# Zamień pojedyncze cudzysłowy
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Wynik: Hello, World!

# Zamień podwójne cudzysłowy
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Wynik: Hello, World!
```

Dla obu typów:

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Zauważ użycie klasy znaków regex
Write-Output $cleanString  # Wynik: Hi there, she said.
```

Przykładowe wyjście z konsoli będzie wyglądać mniej więcej tak:

```
Hello, World!
Hello, World!
Hi there, she said.
```

## Dogłębna analiza
Dawno temu, zanim PowerShell był choćby w planach Microsoftu, przetwarzanie tekstu w Windows często było domeną skryptów wsadowych, które miały ograniczone możliwości. Wprowadzenie PowerShell'a przyniosło potężne funkcje manipulacji tekstem, które uczyniły skryptowanie znacznie bardziej solidnym.

Istnieją alternatywy dla `-replace`, takie jak użycie metody `.Trim()` do usunięcia cudzysłowów tylko na początku i na końcu stringa, ale nie oferują one takiej samej kontroli lub wsparcia regex.

```PowerShell
# Użycie .Trim() dla cudzysłowów na początku i na końcu
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Wynik: Hello, World!
```

Należy zauważyć, że `-replace` używa regex w tle, więc kiedy pracujesz z nim, pamiętaj, że znaki specjalne muszą być poprzedzone ukośnikiem, jeśli masz na celu ich dopasowanie. Jeśli potrzebujesz bardziej szczegółowej kontroli nad usuwaniem cudzysłowów, zagłębienie się w regex z `-replace` jest drogą do tego, dającą ci ogromną elastyczność.

## Zobacz także
- Aby dowiedzieć się więcej o regex w PowerShell, sprawdź oficjalną dokumentację: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Odkryj inne metody stringów: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
