---
date: 2024-01-20 17:32:39.029052-07:00
description: "Por\xF3wnywanie dw\xF3ch dat w C# to sprawdzanie, kt\xF3ra z dw\xF3\
  ch dat jest wcze\u015Bniejsza, p\xF3\u017Aniejsza lub czy s\u0105 identyczne. Programi\u015B\
  ci robi\u0105 to, aby obs\u0142ugiwa\u0107\u2026"
lastmod: '2024-02-25T18:49:33.785470-07:00'
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dw\xF3ch dat w C# to sprawdzanie, kt\xF3ra z dw\xF3ch dat\
  \ jest wcze\u015Bniejsza, p\xF3\u017Aniejsza lub czy s\u0105 identyczne. Programi\u015B\
  ci robi\u0105 to, aby obs\u0142ugiwa\u0107\u2026"
title: "Por\xF3wnywanie dw\xF3ch dat"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Porównywanie dwóch dat w C# to sprawdzanie, która z dwóch dat jest wcześniejsza, późniejsza lub czy są identyczne. Programiści robią to, aby obsługiwać harmonogramy, terminy płatności, logowanie działalności i inne zadania związane z czasem.

## How to: (Jak to zrobić:)
```C#
using System;

class DateComparison
{
    static void Main()
    {
        DateTime date1 = new DateTime(2023, 3, 14);
        DateTime date2 = new DateTime(2023, 5, 18);

        int comparison = DateTime.Compare(date1, date2);

        if (comparison < 0)
            Console.WriteLine($"{date1} is earlier than {date2}");
        else if (comparison == 0)
            Console.WriteLine($"{date1} is the same as {date2}");
        else
            Console.WriteLine($"{date1} is later than {date2}");
    }
}

// Sample output:
// 2023-03-14 00:00:00 is earlier than 2023-05-18 00:00:00
```

## Deep Dive (Głębsze zanurzenie)
Porównywanie dat sięga korzeni programowania – zarządzanie czasem zawsze było kluczowe. W C#, metoda `DateTime.Compare()` służy do tego celu. Możemy też użyć operatorów porównania (`<`, `>`, `==`). Alternatywą jest `TimeSpan`, jeśli interesuje nas różnica w czasie. Implementacja w .NET jest wierna ISO 8601 w obsłudze dat, co jest międzynarodowym standardem.

## See Also (Zobacz również)
- MSDN Documentation on DateTime: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- .NET API DateTime.Compare Method: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare)
- Time Zones in .NET: [https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
