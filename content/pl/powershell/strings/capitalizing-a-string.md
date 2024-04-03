---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.094884-07:00
description: "Jak to zrobi\u0107: PowerShell, b\u0119d\u0105c wszechstronnym narz\u0119\
  dziem, pozwala na u\u017Cycie wielkiej litery w ci\u0105gu znak\xF3w za pomoc\u0105\
  \ prostych metod, bez konieczno\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.610767-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, b\u0119d\u0105c wszechstronnym narz\u0119dziem, pozwala na u\u017C\
  ycie wielkiej litery w ci\u0105gu znak\xF3w za pomoc\u0105 prostych metod, bez konieczno\u015B\
  ci korzystania z bibliotek firm trzecich."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
PowerShell, będąc wszechstronnym narzędziem, pozwala na użycie wielkiej litery w ciągu znaków za pomocą prostych metod, bez konieczności korzystania z bibliotek firm trzecich. Oto jak możesz to zrobić:

```powershell
# Korzystanie z wbudowanej metody .Net 'ToTitleCase' z CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Wynik:
```
Hello world
```

Uwaga: Ta metoda zmienia na wielką literę pierwszą literę każdego słowa. Jeśli chcesz wyraźnie zmienić tylko pierwszą literę ciągu i pozostawić resztę bez zmian, możesz zrobić coś takiego:

```powershell
# Użycie wielkiej litery tylko dla pierwszego znaku ciągu
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Wynik:
```
Hello world
```

PowerShell bezpośrednio nie zawiera prostej funkcji do używania wielkiej litery tylko dla pierwszej litery ciągu, ale łącząc podstawowe metody manipulacji ciągiem, takie jak `Substring(0,1).ToUpper()` i konkatenację, możemy łatwo osiągnąć pożądany rezultat.
