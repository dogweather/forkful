---
title:    "C#: Beräkning av ett datum i framtiden eller i det förflutna"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Ibland kan vi vilja veta vilket datum det kommer att vara om ett visst antal dagar eller från en viss tidpunkt i det förflutna. Om vi t.ex. vill planera en semester eller födelsedagsfest kan det vara användbart att kunna beräkna datumet i förväg.

## Hur man gör

För att beräkna ett datum i framtiden eller i det förflutna, kan vi använda DateTime-objektet i C#. Detta objekt har inbyggda metoder för att manipulera datum och tid.

För att beräkna ett datum i framtiden, använder vi Add-metoden och anger antalet dagar som vi vill lägga till. I följande exempel beräknas datumet 60 dagar framåt från dagens datum:

```C#
DateTime datum = DateTime.Now;
datum = datum.Add(TimeSpan.FromDays(60));
Console.WriteLine("Datumet 60 dagar framåt är: " + datum.ToString("dd/MM/yyyy"));
```

Output: Datumet 60 dagar framåt är: 14/11/2021

För att beräkna ett datum i det förflutna kan vi använda Subtract-metoden och ange antalet dagar som vi vill dra bort. I följande exempel beräknas datumet 200 dagar bakåt från dagens datum:

```C#
DateTime datum = DateTime.Now;
datum = datum.Subtract(TimeSpan.FromDays(200));
Console.WriteLine("Datumet 200 dagar bakåt är: " + datum.ToString("dd/MM/yyyy"));
```

Output: Datumet 200 dagar bakåt är: 28/03/2021

## Djupdykning

Det är viktigt att komma ihåg att DateTime-objektet även har möjligheten att beräkna datum med hjälp av veckor, månader och år. Vi kan använda AddWeeks, AddMonths och AddYears-metoderna för att lägga till ett specifikt antal veckor, månader eller år till ett datum.

Det är också möjligt att beräkna skillnaden mellan två datum med hjälp av Subtract-metoden. Detta är användbart om vi till exempel vill veta hur många dagar det är kvar till en viss händelse.

## Se även

* [DateTime-strukturen i C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
* [DateTime-metoder för datumberäkning](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.add?view=net-5.0)
* [Subtract-metoden för DateTime-objekt](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.subtract?view=net-5.0)