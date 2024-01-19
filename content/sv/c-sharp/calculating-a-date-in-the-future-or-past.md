---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "C#: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller förflutna innebär att lägga till eller dra från ett specifikt tal av dagar, månader eller år från ett existerande datum. Det blir ofta tillämpat inom kodning för att manipulera data eller relevanta tidslinjer.

## Så här gör du:

Här är en exempel på hur detta skulle kunna genomföras i C#:

```C#
//Vi använder DateTime biblioteket
using System;

namespace Date_Time_Manipulation
{
    class Program
    {
        static void Main()
        {
            // Hämtar dagens datum
            DateTime today = DateTime.Now;
            // Skriver ut dagens datum
            Console.WriteLine("Idag är det: " + today.ToString());
 
            // Beräknar datumet för en vecka från idag
            DateTime NextWeek = today.AddDays(7);
            Console.WriteLine("Datumet för en vecka från idag är: " + NextWeek.ToString());
        }
    }
}
```

Efter att du kört programmet får du ut följande output:

```
Idag är det: 2022-03-30 12:43:08
Datumet för en vecka från idag är: 2022-04-06 12:43:08
```
 
## Djupdykning:

(1) Historiskt så skapades DateTime-objektet i C# för att hantera data och tid. Den har blivit ett viktigt verktyg för att till exempel beräkna tidsskillnader och att boka scheman.

(2) Det finns olika alternativ för att beräkna ett datum i framtiden eller förflutit. Ett sätt att göra det är att använda DateTime och dess metoder, som vi gjorde i exemplet ovan, men det finns också alternativ som NodaTime-biblioteket.

(3) För att implementera DateTime i din kod behöver du förstå vilka metoder som finns tillgängliga för dig. De mest använda är AddDays, AddMonths och AddYears. Men det finns också mer specifika metoder som AddHours, AddMinutes och AddSeconds. 

## Se också:

- Microsofts officiella dokumentation på DateTime: 
  [https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
  
- NodaTime-biblioteket för mer avancerade datum- och tidshantering:
  [https://nodatime.org/](https://nodatime.org/)  

Kom ihåg, oavsett vilket verktyg eller metoden du väljer, är det viktigaste att det passar dina behov och din kodningsstil.