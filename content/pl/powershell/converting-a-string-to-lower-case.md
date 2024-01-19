---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zamiana łańcucha znaków na małe litery oznacza przekształcenie wszystkich liter alfabetu w łańcuchu na ich mało literowe odpowiedniki. Programiści robią to, aby ułatwić porównanie i sortowanie napisów, ponieważ operacje te często są niezależne od wielkości liter.

## Jak to zrobić:
Napisy w PowerShell możemy zapisać małymi literami za pomocą metody `.ToLower()`. Oto przykład:
```PowerShell
$mojString = "HeLLo PoWeRShELL"
$mojString.ToLower()
```
Po uruchomieniu powyższego kodu output będzie następujący:
```PowerShell
"hello powershell"
```

## Głębsze Zanurzenie:
Metoda `.ToLower()` była dostępna od wczesnych wersji języków programowania .NET, w tym C# i Visual Basic. Alternatywą jest użycie metody `.ToLowerInvariant()`, która zapewnia konsekwencje między różnymi ustawieniami języka.

Co więcej, szczegółowa implementacja `.ToLower()` zależy od konkretnego .NET Runtime'a. W środowisku .NET Framework, ta metoda opiera się na informacjach zawartych w "TextInfo" dla aktualnej kultury.

## Zobacz także:
- Dokumentacja Microsoft na temat metody `.ToLower()`: [link](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- Poradnik Microsoft na temat sortowania łańcuchów znaków: [link](https://docs.microsoft.com/en-us/dotnet/standard/base-types/comparing-strings?view=net-5.0)
- Dokumentacja Microsoft na temat znaków niezależnych od kultury: [link](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings?view=net-5.0)