---
title:                "C#: Jämförelse av två datum"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man arbetar med datum- och tidsdata. Det kan vara användbart för att kontrollera om ett visst datum är före eller efter ett annat datum, eller för att beräkna skillnaden mellan två datum.

## Så här gör du

För att jämföra två datum i C# behöver vi först skapa två DateTime-objekt som innehåller de datum vi vill jämföra. Sedan kan vi använda olika metoder för att utföra olika jämförelser.

```C#
DateTime date1 = new DateTime(2021, 10, 15);
DateTime date2 = new DateTime(2021, 10, 20);

// Kontrollera om date1 är tidigare än date2
if(date1 < date2)
{
    Console.WriteLine("date1 är tidigare än date2");
}

// Kontrollera om date1 är senare än date2
if(date1 > date2)
{
    Console.WriteLine("date1 är senare än date2");
}

// Kontrollera om date1 är samma som date2
if(date1 == date2)
{
    Console.WriteLine("date1 är samma som date2");
}

// Beräkna antalet dagar mellan date1 och date2
int dagar = (date2 - date1).Days;
Console.WriteLine("Skillnaden mellan date1 och date2 är " + dagar + " dagar");
```

#### Exempeloutput:

```
date1 är tidigare än date2
Skillnaden mellan date1 och date2 är 5 dagar
```

## Fördjupning

När vi jämför datum i C# jämförs inte bara de faktiska datumen, utan även tiden. Det betyder att om vi har två DateTime-objekt med samma datum, men olika tider, kommer de ändå att vara olika när vi jämför dem.

För att undvika detta kan vi använda metoden `DateTime.Date` för att endast jämföra datumen och ignorera tiderna.

För mer avancerad hantering av datum och tider finns också klassen `TimeSpan` som kan användas för att beräkna skillnaden mellan två datum och tider med högre precision.

## Se också

- [DateTime-struktur (C#-programmeringsguide)](https://docs.microsoft.com/sv-se/dotnet/csharp/language-reference/builtin-types/struct)
- [DateTime.Compare-metoden (C#-programmeringsguide)](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime.compare)
- [TimeSpan-struktur (C#-programmeringsguide)](https://docs.microsoft.com/sv-se/dotnet/api/system.timespan)