---
title:                "Kahden päivämäärän vertailu"
aliases:
- /fi/c-sharp/comparing-two-dates.md
date:                  2024-01-20T17:32:32.610973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Vertaillaan kahta päivämäärää tietääksemme, kumpi on ensin tai paljonko aikaa on kulunut niiden välillä. Ohjelmoijat tekevät tämän hallitakseen aikatauluja, ajoituksia ja määräaikoja.

## How to: (Kuinka tehdään:)
```C#
using System;

class DatesComparison
{
    static void Main()
    {
        DateTime startDate = new DateTime(2023, 4, 1);
        DateTime endDate = new DateTime(2023, 4, 30);

        // Vertaillaan päivämääriä
        int result = DateTime.Compare(startDate, endDate);

        // Tulostetaan tulos
        if(result < 0)
            Console.WriteLine($"{startDate:d} on ennen {endDate:d}.");
        else if(result == 0)
            Console.WriteLine($"{startDate:d} ja {endDate:d} ovat samana päivänä.");
        else
            Console.WriteLine($"{startDate:d} on jälkeen {endDate:d}.");

        // Ero päivissä
        TimeSpan dateDifference = endDate - startDate;
        Console.WriteLine($"Ero on {dateDifference.Days} päivää.");
    }
}
```
Sample output:
```
1.4.2023 on ennen 30.4.2023.
Ero on 29 päivää.
```

## Deep Dive (Sukellus syvyyksiin):
Ennen `DateTime`-luokkaa ohjelmoijat joutuivat käsittelemään aikoja itse, mikä oli altista virheille. `DateTime.Compare` on yksinkertainen ja tarkka tapa vertailla ajanhetkiä C#:ssa. Tämä metodi palauttaa kokonaisluvun, joka kertoo järjestyksen. Vaihtoehtoisesti, voit käyttää `TimeSpan`-luokkaa aikaeron mittaamiseen. Nämä työkalut ovat osa .NETin laajempaa päivämäärä- ja aikahallintaa.

## See Also (Katso Myös):
- Microsoft Docs, DateTime.Compare: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare
- Microsoft Docs, TimeSpan: https://docs.microsoft.com/en-us/dotnet/api/system.timespan
- C# Date and Time tutorial: https://www.tutorialspoint.com/csharp/csharp_date_time.htm
