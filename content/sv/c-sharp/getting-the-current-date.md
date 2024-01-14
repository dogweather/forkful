---
title:                "C#: Att få aktuellt datum"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få den nuvarande datumen kan vara användbart för olika program och applikationer. Det kan användas för att visa den aktuella tidpunkten i en kalenderapplikation eller för att hålla reda på när en transaktion gjordes i ett ekonomisystem.

## Hur man
För att få den aktuella datumen i C#, kan du använda DateTime-objektet.

```C#
DateTime currentDateTime = DateTime.Now;
Console.WriteLine("Aktuellt datum: " + currentDateTime.ToString("dd/MM/yyyy"));
Console.WriteLine("Aktuell tid: " + currentDateTime.ToString("HH:mm:ss"));
```

Output:
```
Aktuellt datum: 21/10/2020
Aktuell tid: 09:00:00
```
Först skapar vi ett DateTime-objekt och tilldelar det den aktuella datumen med DateTime.Now. Sedan använder vi ToString-metoden för att formatera datumet och tiden enligt vår önskade format.

## Djupdykning
DateTime-objektet i C# är mycket kraftfullt och innehåller många metoder och egenskaper för att hantera datum och tid. Du kan till exempel använda Add-metoden för att lägga till eller subtrahera en viss tid till det aktuella datumet eller använda Parse-metoden för att konvertera en sträng till ett DateTime-objekt.

Nedan är några andra användbara metoder och egenskaper som finns tillgängliga:

- `DateTime.Today`: Returnerar en DateTime-objekt som innehåller datumen för dagens datum.
- `DateTime.IsLeapYear(year)`: Returnerar en boolesk värde som indikerar om det angivna året är ett skottår.
- `DateTime.Compare(date1, date2)`: Jämför två DateTime-objekt och returnerar en positiv, negativ eller noll värde baserat på vilket datum som är senare.
- `DateTime.TryParse(str, out date)`: Försöker tolka en sträng som ett DateTime-objekt och returnerar en boolesk värde som indikerar om tolkningen var framgångsrik eller inte.

## Se även
- [Microsoft Docs: DateTime Struct] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [C# DateTime Example] (https://www.c-sharpcorner.com/article/c-sharp-datetime-examples/)
- [DateTime vs DateTimeOffset] (https://www.c-sharpcorner.com/article/datetime-vs-datetimeoffset/)