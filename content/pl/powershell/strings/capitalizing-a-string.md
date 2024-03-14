---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.094884-07:00
description: "U\u017Cycie wielkiej litery w ci\u0105gu znak\xF3w w PowerShellu polega\
  \ na przekszta\u0142ceniu pierwszego znaku danego ci\u0105gu na wielk\u0105 liter\u0119\
  , pozostawiaj\u0105c reszt\u0119 ci\u0105gu\u2026"
lastmod: '2024-03-13T22:44:35.610767-06:00'
model: gpt-4-0125-preview
summary: "U\u017Cycie wielkiej litery w ci\u0105gu znak\xF3w w PowerShellu polega\
  \ na przekszta\u0142ceniu pierwszego znaku danego ci\u0105gu na wielk\u0105 liter\u0119\
  , pozostawiaj\u0105c reszt\u0119 ci\u0105gu\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Użycie wielkiej litery w ciągu znaków w PowerShellu polega na przekształceniu pierwszego znaku danego ciągu na wielką literę, pozostawiając resztę ciągu niezmienioną. Programiści często wykonują to zadanie w celach formatowania, takich jak przygotowanie tekstu do wyświetlenia w interfejsach użytkownika lub przestrzeganie reguł gramatycznych w generowanych dokumentach.

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
