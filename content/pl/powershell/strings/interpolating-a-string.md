---
date: 2024-01-20 17:51:22.918258-07:00
description: "Interpolacja napis\xF3w to wstawianie warto\u015Bci wyra\u017Ce\u0144\
  \ do napisu. Programi\u015Bci u\u017Cywaj\u0105 tego, aby tworzy\u0107 tekst z dynamicznie\
  \ zmieniaj\u0105cymi si\u0119 danymi, co\u2026"
lastmod: '2024-03-13T22:44:35.613683-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja napis\xF3w to wstawianie warto\u015Bci wyra\u017Ce\u0144 do\
  \ napisu. Programi\u015Bci u\u017Cywaj\u0105 tego, aby tworzy\u0107 tekst z dynamicznie\
  \ zmieniaj\u0105cymi si\u0119 danymi, co\u2026"
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## What & Why? (Co i Dlaczego?)
Interpolacja napisów to wstawianie wartości wyrażeń do napisu. Programiści używają tego, aby tworzyć tekst z dynamicznie zmieniającymi się danymi, co ułatwia czytelność i utrzymanie kodu.

## How to: (Jak to zrobić:)
```PowerShell
# Tworzenie zmiennych
$userName = "Bartek"
$dayOfWeek = Get-Date -Format "dddd"

# Interpolacja napisu
$text = "Cześć, $userName! Dziś jest $dayOfWeek."
Write-Output $text
```
Wynik:
```
Cześć, Bartek! Dziś jest środa.
```

Dodatkowy przykład z użyciem wyrażenia w środku napisu:
```PowerShell
$hoursToMeeting = 3
$text = "Do spotkania pozostało: $($hoursToMeeting * 60) minut."
Write-Output $text
```
Wynik:
```
Do spotkania pozostało: 180 minut.
```

## Deep Dive (Dogłębna analiza)
Interpolacja napisów nie zawsze była dostępna w językach programowania. W PowerShell pojawiła się jako część 'expandable strings', gdzie $ można użyć do osadzenia zmiennej wewnątrz napisu. Jeśli potrzebujesz umieścić bardziej skomplikowane wyrażenia, używaj `$()` wewnątrz napisu.

Alternatywą w PowerShell jest użycie operatora plus (`+`) do łączenia napisów z wartościami, ale to zazwyczaj mniej wygodne niż interpolacja. Naprzykład:
```PowerShell
# Przykład bez interpolacji
$text = "Cześć, " + $userName + "! Dziś jest " + $dayOfWeek + "."
Write-Output $text
```

Interpolacja jest implementowana tak, że w czasie działania programu, środowisko wykonawcze zastępuje wyrażenie zawarte między `$` i końcem napisu lub zamkniętym nawiasem `$()` wartością tej zmiennej lub wyrażenia.

## See Also (Zobacz również)
- [Microsoft's PowerShell documentation](https://docs.microsoft.com/en-us/powershell/)
