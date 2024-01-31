---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:28:33.019139-07:00
model:                 gpt-4-1106-preview
html_title:           "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest obliczanie daty w przyszłości lub przeszłości? To po prostu wyliczanie nowej daty, dodając lub odejmując czas od danej daty początkowej. Programiści robią to często – zarządzają terminami, planują zdarzenia, tworzą logi.

## How to:
C# ma mocne wsparcie dla operacji na datach. Spójrz na poniższe przykłady.

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        DateTime nextWeek = today.AddDays(7);
        DateTime lastMonth = today.AddMonths(-1);
        
        Console.WriteLine("Dzisiaj: " + today.ToShortDateString());
        Console.WriteLine("Za tydzień będzie: " + nextWeek.ToShortDateString());
        Console.WriteLine("Miesiąc temu było: " + lastMonth.ToShortDateString());
    }
}
```

Sample output:
```
Dzisiaj: 12.03.2023
Za tydzień będzie: 19.03.2023
Miesiąc temu było: 12.02.2023
```

## Deep Dive
Zanim dołączono klasę `DateTime` do .NET, obliczenia na datach wymagały ręcznego zarządzania czasem – co było skomplikowane i narażone na błędy. `DateTime` uprościło zadania i jest częścią .NET od samego początku.

Inne opcje to `TimeSpan` dla różnic czasowych, a `DateTimeOffset` dla operacji świadomych strefy czasowej. We współczesnym C#, można także używać biblioteki NodaTime dla bardziej zaawansowanych scenariuszy związanych z czasem.

Szczegóły implementacji? Klasa `DateTime` reprezentuje punkt w czasie, dokładność do 100-nanosekund, a `DateTimeKind` rozróżnia czas uniwersalny (UTC) od lokalnego. Pamiętaj też o sprawdzeniu ustawień regionalnych – formatowanie daty może się różnić.

## See Also
- Dokumentacja Microsoft: [DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Poradnik MSDN: [Standard DateTime Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- Projekt NodaTime: [http://nodatime.org/](http://nodatime.org/)
- Stack Overflow: dyskusje i pytania dotyczące `DateTime` w C#
