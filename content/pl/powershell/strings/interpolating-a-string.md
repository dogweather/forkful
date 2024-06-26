---
date: 2024-01-20 17:51:22.918258-07:00
description: "How to: (Jak to zrobi\u0107:) Interpolacja napis\xF3w nie zawsze by\u0142\
  a dost\u0119pna w j\u0119zykach programowania. W PowerShell pojawi\u0142a si\u0119\
  \ jako cz\u0119\u015B\u0107 'expandable\u2026"
lastmod: '2024-04-05T22:50:49.939618-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Interpolacja napis\xF3w nie zawsze by\u0142a dost\u0119\
  pna w j\u0119zykach programowania."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

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
