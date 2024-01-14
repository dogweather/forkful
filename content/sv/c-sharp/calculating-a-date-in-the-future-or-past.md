---
title:                "C#: Beräkning av ett datum i framtiden eller förflutnan"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika situationer där det kan vara nödvändigt att kunna räkna ut ett datum långt in i framtiden eller ett datum som har passerat. Till exempel när man bokar en resa eller planerar ett evenemang. Därför är det användbart att kunna använda sig av C# för att räkna ut sådana datum.

## Så här gör man

För att kunna räkna ut ett datum i framtiden eller förflutet i C#, behöver man först och främst känna till den nuvarande dagen. Detta kan man enkelt få genom att använda DateTime.Now-funktionen. Sedan kan man använda sig av C# Date and Time-funktionerna för att utföra olika beräkningar och få fram det datum man önskar.

```C#
// Kodexempel för att räkna ut ett datum 30 dagar framåt i tiden

DateTime nu = DateTime.Now;
DateTime omTrettioDagar = nu.AddDays(30);
Console.WriteLine("Datumet om trettio dagar är: " + omTrettioDagar.ToShortDateString());

// Output: Datumet om trettio dagar är: xx/xx/xxxx (30 dagar framåt i tiden)

// Kodexempel för att räkna ut ett datum 60 dagar bakåt i tiden

DateTime nu = DateTime.Now;
DateTime forraManad = nu.AddMonths(-2);
Console.WriteLine("Datumet för två månader sedan var: " + forraManad.ToShortDateString());

// Output: Datumet för två månader sedan var: xx/xx/xxxx (60 dagar tillbaka i tiden)
```

## Djupdykning

I C# finns det olika funktioner man kan använda sig av för att räkna ut både datum i framtiden och förflutet. En av de vanligaste är DateTime-funktionen .AddDays(), som låter oss lägga till eller subtrahera ett visst antal dagar från ett visst datum. Man kan också använda sig av .AddMonths() och .AddYears() för att räkna ut datum baserat på månader eller år.

Det finns också möjlighet att använda sig av .AddHours() och .AddMinutes() för att räkna ut ett tidsspann framåt eller bakåt i tiden.

## Se även

- [Microsoft C# DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# Date and Time Functions](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [DateTime.AddHours() Method](https://www.geeksforgeeks.org/c-sharp-datetime-addhours-method/)