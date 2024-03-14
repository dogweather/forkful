---
date: 2024-01-26 03:41:13.742061-07:00
description: "Usuwanie cudzys\u0142ow\xF3w ze stringa w PowerShellu polega na wyeliminowaniu\
  \ pojedynczych (`'`) lub podw\xF3jnych (`\"`) znak\xF3w cudzys\u0142owu otaczaj\u0105\
  cych tekst.\u2026"
lastmod: '2024-03-13T22:44:35.615764-06:00'
model: gpt-4-0125-preview
summary: "Usuwanie cudzys\u0142ow\xF3w ze stringa w PowerShellu polega na wyeliminowaniu\
  \ pojedynczych (`'`) lub podw\xF3jnych (`\"`) znak\xF3w cudzys\u0142owu otaczaj\u0105\
  cych tekst.\u2026"
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów ze stringa w PowerShellu polega na wyeliminowaniu pojedynczych (`'`) lub podwójnych (`"`) znaków cudzysłowu otaczających tekst. Programiści często muszą oczyścić stringi do przetwarzania, porównywania lub w celach wyjściowych, szczególnie kiedy mają do czynienia z danymi wprowadzanymi przez użytkownika lub parsowaniem plików.

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
