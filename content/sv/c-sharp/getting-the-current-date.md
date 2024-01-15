---
title:                "Få den aktuella datumet"
html_title:           "C#: Få den aktuella datumet"
simple_title:         "Få den aktuella datumet"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hämta den nuvarande datumet är en väldigt användbar färdighet när du programmerar. Det kan hjälpa dig att hålla koll på när ditt program kördes och användas för datasortering och tidsberäkningar.

## Så här gör du
För att hämta den aktuella datumet i C# kan du använda DateTime klassen. Du kan använda metoden Now() för att få den aktuella tiden och Datum egenskapen för att få datumet.

```
DateTime nuvarandeDatum = DateTime.Now;
Console.WriteLine("Det aktuella datumet är: " + nuvarandeDatum.Datum);
```

Detta kodexempel kommer att skriva ut något som liknar detta:

```
Det aktuella datumet är: 11/11/2021
```

Du kan också formatera datumet på olika sätt genom att använda ToString() metoden tillsammans med en specifik formatsträng. Här är några exempel:

```
nuvarandeDatum.ToString("d"); // skriver ut datumet i kort datumformatet (11/11/2021)
nuvarandeDatum.ToString("D"); // skriver ut datumet i långt datumformat (11 november 2021)
nuvarandeDatum.ToString("yyyy-MM-dd"); // skriver ut datumet i ISO 8601 format (2021-11-11)
nuvarandeDatum.ToString("h:mm:ss tt"); // skriver ut tiden i 12-timmarsformat med AM/PM (11:23:35 PM)
```

## Djupdykning
Om du vill ha mer kontroll över datumet kan du använda DateTime klassens egenskaper, som Dag, Månad och År, för att hämta specifika delar av datumet. Du kan också använda metoder som AddDays() och AddMonths() för att göra beräkningar med datumet. Här är ett exempel:

```
DateTime nuvarandeDatum = DateTime.Now;
Console.WriteLine("Idag är det: " + nuvarandeDatum.DayOfWeek); // skriver ut dagens veckodag (t.ex. torsdag)

Console.WriteLine("Om en vecka är det: " + nuvarandeDatum.AddDays(7).ToString("d")); // lägger till 7 dagar till datumet och skriver ut det i kort datumformatet
```

Dessutom kan du använda DateTimeOffset klassen för att arbeta med datum och tid i olika tidszoner.

## Se också
- [DateTime-klassens dokumentation på Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [DateTimeOffset-klassens dokumentation på Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-6.0)
- [Formatsträngar för datum och tid i C# på Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)