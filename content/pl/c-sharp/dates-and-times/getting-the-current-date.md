---
title:                "Pobieranie aktualnej daty"
date:                  2024-02-03T19:09:16.021710-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie bieżącej daty w C# polega na uzyskaniu obecnych informacji o dacie i czasie z systemu. Programiści często muszą uzyskać dostęp do tych informacji w celu logowania, znakowania czasowego operacji lub planowania zadań w aplikacjach, zapewniając, że działania są wykonywane z odpowiednią dokładnością czasu i dane są oznaczone precyzyjnymi znacznikami czasu.

## Jak to zrobić:
C# oferuje prosty sposób na pobranie bieżącej daty za pomocą klasy `DateTime`, która jest częścią przestrzeni nazw System w .NET Framework. Poniższy przykład demonstruje, jak pobrać bieżącą datę, i opcjonalnie, czas.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Pobiera tylko bieżącą datę
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Wynik: MM/dd/yyyy
        
        // Pobiera bieżącą datę i czas
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Wynik: MM/dd/yyyy HH:mm:ss

        // Pobiera bieżącą datę i czas UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Wynik: MM/dd/yyyy HH:mm:ss
    }
}
```

Jeśli chodzi o biblioteki stron trzecich, NodaTime oferuje solidną alternatywę dla manipulacji datą i czasem, w tym pobieranie bieżącej daty w różnych kalendarzach i strefach czasowych.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Korzystanie z NodaTime do pobrania bieżącej daty w kalendarzu ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Wynik: yyyy-MM-dd

        // Dla dat specyficznych dla strefy czasowej
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Wynik: yyyy-MM-dd
    }
}
```

Pokazuje to podstawowe użycie z wbudowaną klasą `DateTime` oraz zaawansowane możliwości oferowane przez NodaTime, szczególnie przydatne dla aplikacji wymagających obsługi różnych stref czasowych lub systemów kalendarzowych.
