---
title:                "Zmiana wielkości litery w ciągu znaków"
html_title:           "PowerShell: Zmiana wielkości litery w ciągu znaków"
simple_title:         "Zmiana wielkości litery w ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Za pomocą metody string.ToUpper () w PowerShellu można zmienić wszystkie litery ciągu na wielkie litery. Programiści często stosują ten proces do formatowania danych lub porównywania ich z innymi ciągami.

## Jak to zrobić:
```PowerShell
# Przykładowy ciąg:
$string = "cześć! to jest przykładowy string."

# Użycie metody string.ToUpper () na ciągu:
$string.ToUpper()

# Wynik:
CZEŚĆ! TO JEST PRZYKŁADOWY STRING.
```

## Głębsza przeprawa:
1. Metoda string.ToUpper () jest dostępna w wielu językach programowania i jest powszechnie stosowana do manipulacji ciągami.
2. Alternatywą dla tej metody w PowerShellu jest użycie operatora -ceq, który porównuje dwa ciągi bez uwzględniania wielkości liter.
3. Implementacja metody string.ToUpper () polega na wywołaniu funkcji ToUpper () z klasy string, która jest częścią przestrzeni nazw System. Jest to także jakaś polemika na temat wykorzystania CLR w PowerShellu.

## Zobacz także:
- [Dokumentacja Microsoftu na temat metody string.ToUpper ()](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.toupper)
- [Porównywanie ciągów w PowerShellu](https://devblogs.microsoft.com/scripting/comparing-strings-in-powershell/)
- [Wykorzystanie CLR w PowerShellu](https://docs.microsoft.com/pl-pl/dotnet/framework/interop/using-powershell-to-interop-with-com)