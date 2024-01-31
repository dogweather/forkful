---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:35:48.173074-07:00
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zamiana ciągu znaków na datę to proces przekształcenia tekstu zawierającego datę w strukturalny format daty/czasu. Programiści robią to, aby łatwiej zarządzać i manipulować datami w aplikacjach.

## How to: (Jak to zrobić:)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023-04-01";
        DateTime convertedDate;
        
        // Użycie domyślnych ustawień systemowych
        convertedDate = DateTime.Parse(dateString);
        Console.WriteLine($"Domyślne ustawienia: {convertedDate}");

        // Użycie konkretnego formatu
        string format = "yyyy-MM-dd";
        CultureInfo provider = CultureInfo.InvariantCulture;
        convertedDate = DateTime.ParseExact(dateString, format, provider);
        Console.WriteLine($"Format wybrany: {convertedDate}");
    }
}
```

Sample output (Przykładowe wyjście):
```
Domyślne ustawienia: 2023-04-01 00:00:00
Format wybrany: 2023-04-01 00:00:00
```

## Deep Dive (Dogłębna analiza):
Historia: W C# od wczesnych wersji istnieje możliwość parsowania dat. `DateTime.Parse` i `DateTime.ParseExact` to metody, które ewoluowały, ale ich podstawowa funkcjonalność pozostała niezmieniona.

Alternatywa: Poza standardowym `DateTime` jest `DateTimeOffset`, które dodatkowo uwzględnia strefę czasową, oraz nowszy `System.Globalization.CultureInfo` do obsługi różnych formatów regionalnych.

Szczegóły implementacyjne: `DateTime.ParseExact` wymaga określenia konkretnego wzorca daty, co daje kontrolę nad formatem. `CultureInfo.InvariantCulture` pozwala uniknąć problemów z różnicami regionalnymi.

## See Also (Zobacz również):
- [Dokumentacja DateTime.Parse](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parse?view=netframework-4.8)
- [Dokumentacja DateTime.ParseExact](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact?view=netframework-4.8)
- [Przewodnik po formatach daty i czasu w .NET](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Różnice pomiędzy DateTime a DateTimeOffset](https://docs.microsoft.com/pl-pl/dotnet/standard/datetime/choosing-between-datetime)
