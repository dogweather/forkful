---
title:                "Zaokrąglanie liczb"
aliases:
- pl/powershell/rounding-numbers.md
date:                  2024-01-26T03:46:26.530789-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb polega na dostosowaniu wartości do najbliższej liczby całkowitej lub określonego miejsca dziesiętnego. Programiści zaokrąglają liczby, aby upraszczać dane, poprawiać czytelność lub spełniać pewne wymagania matematyczne podczas obliczeń.

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
