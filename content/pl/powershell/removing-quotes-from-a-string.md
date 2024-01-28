---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:41:13.742061-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/removing-quotes-from-a-string.md"
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
