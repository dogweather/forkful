---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "PowerShell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu dni, tygodni, miesięcy czy lat od podanej daty. Programiści korzystają z tego, gdy potrzebują harmonogramów, ustalania terminów wygaśnięcia licencji, przypomnień itp.

## Jak to zrobić:
Podstawowe obliczanie daty w przyszłości lub przeszłości za pomocą PowerShell’a można osiągnąć, korzystając z metody ‘AddDays’. Przykład poniżej pokazuje dodawanie 7 dni do obecnej daty:
```PowerShell
$Date = Get-Date
$NewDate = $Date.AddDays(7)
$NewDate
```
Wynikiem będzie data o 7 dni późniejsza od obecnej. Podobnie, możemy odejmować dni:

```PowerShell
$Date = Get-Date
$NewDate = $Date.AddDays(-7)
$NewDate
```
Tu wynikiem będzie data o 7 dni wcześniejsza niż obecna.

## W głąb tematu:
Technika obliczania daty w przyszłości lub przeszłości ma swoje korzenie w tradycyjnymi operacjach matematycznych, ale jest ona znacznie bardziej rozbudowana, biorąc pod uwagę złożoność kalendarzy. PowerShell posiada podstawowe metody takie jak ‘AddDays’, ‘AddHours’, ‘AddMinutes’, ‘AddSeconds’, ‘AddMilliseconds’, ‘AddTicks’, ‘AddYears’, ‘AddMonths’. Alternatywą dla PowerShell jest korzystanie z języków jak C# lub Python, które oferują podobne funkcje.

## Zobacz też:
- Dokumentacja PowerShell AddDays: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1
- Cheatsheet daty i czasu w PowerShell: https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-add-days-hours-minutes-to-date/
- Obsługa daty i czasu w .NET: https://docs.microsoft.com/dotnet/api/system.datetime?view=net-5.0