---
title:    "C#: Konvertering av datum till en sträng"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift som många C#-utvecklare stöter på. Det kan vara till nytta när man vill visa ett datum på ett läsbart sätt eller när man behöver spara det som en sträng i en databas. I denna bloggpost kommer vi att titta närmare på hur man gör detta på ett enkelt sätt.

## Hur man gör det

För att konvertera ett datum till en sträng använder man sig av metoden `ToString()` tillsammans med ett formatstring. Formatstringen bestämmer hur datumet ska visas som en sträng, till exempel år-månad-datum eller månad-dag-år. Nedan följer ett exempel på hur man kan göra detta:

```C#
DateTime datum = new DateTime(2020, 6, 1);
string datumSträng = datum.ToString("yyyy-MM-dd");
Console.WriteLine(datumSträng);
```

Detta kommer att ge följande output:

`2020-06-01`

Som du kan se så har vi använt oss av `ToString()`-metoden tillsammans med formatstringen "yyyy-MM-dd". Det finns många olika format som kan användas för att konvertera datum till strängar, så det är bra att titta på dokumentationen för att hitta den bästa lösningen för ditt specifika behov.

## Djupdykning

När man använder `ToString()`-metoden för att konvertera ett datum till en sträng, så används standardformatet för det specifika datumobjektet. Det betyder att om du inte anger en formatstring, så kommer datumet att visas på det sätt som det är definierat i din dator.

Det finns också vissa fördefinierade format som kan användas för att konvertera datum, till exempel `ToShortDateString()` och `ToLongDateString()`. Dessa använder sig av standardformatet för det specifika datumobjektet, men ger dig lite mer kontroll över hur det visas.

Om du behöver mer flexibilitet när det kommer till format för konvertering av datum, så finns det även möjlighet att skapa egna format som passar dina behov. Detta kan göras genom att använda sig av `ToString()`-metoden tillsammans med speckar tecken (#) och skräddarsydda formatsträngar.

## Se även

- [DateTime.ToString() dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=netcore-3.1)
- [Custom Date and Time Format Strings dokumentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)