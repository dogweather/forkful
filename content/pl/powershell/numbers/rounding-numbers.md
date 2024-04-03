---
date: 2024-01-26 03:46:26.530789-07:00
description: "Jak to zrobi\u0107: W PowerShellu masz kilka przydatnych polece\u0144\
  \ cmdlet i metod do zaokr\u0105glania: - Metoda `Round()` z klasy Math."
lastmod: '2024-03-13T22:44:35.622872-06:00'
model: gpt-4-0125-preview
summary: "W PowerShellu masz kilka przydatnych polece\u0144 cmdlet i metod do zaokr\u0105\
  glania."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
W PowerShellu masz kilka przydatnych poleceń cmdlet i metod do zaokrąglania:

- Metoda `Round()` z klasy Math
```PowerShell
[Math]::Round(15.68) # Zaokrągla do 16
```
- Określanie miejsc dziesiętnych:
```PowerShell
[Math]::Round(15.684, 2) # Zaokrągla do 15.68
```
- `Ceiling()` i `Floor()`, zawsze zaokrąglając w górę lub w dół:
```PowerShell
[Math]::Ceiling(15.2) # Zaokrągla w górę do 16
[Math]::Floor(15.9) # Zaokrągla w dół do 15
```

## Dogłębnie
Zaokrąglanie liczb nie jest nowością; istnieje od czasów starożytnych, przydatne w handlu, nauce i pomiarze czasu. Mówiąc o PowerShellu, `[Math]::Round()` domyślnie stosuje "Zaokrąglanie bankowe", gdzie 0,5 kierowane jest do najbliższej parzystej liczby, co redukuje błędy w operacjach statystycznych.

Nie jesteś jednak ograniczony tylko do metod `[Math]`. Chcesz więcej kontroli? Sprawdź `[System.Math]::Round(Number, Digits, MidpointRounding)`, gdzie możesz ustawić sposób traktowania punktów środkowych: od zera lub do parzystej (czyli zaokrąglanie bankowe).

Inna perspektywa: obiekt `System.Globalization.CultureInfo`. Pomaga w lokalnospecyficznym formatowaniu i preferencjach zaokrąglania, gdy zajmujesz się międzynarodowymi liczbami.

## Zobacz także
- Oficjalna dokumentacja Microsoft na temat metod Math: [Link](https://learn.microsoft.com/pl-pl/dotnet/api/system.math?view=net-7.0)
- Specyfika zaokrąglania liczb dziesiętnych w .NET: [Link](https://learn.microsoft.com/pl-pl/dotnet/api/system.midpointrounding?view=net-7.0)
- Dyskusje na temat zaokrąglania w StackOverflow: [Link](https://stackoverflow.com/questions/tagged/rounding+powershell)
