---
title:                "C#: Att få nuvarande datum"
simple_title:         "Att få nuvarande datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att veta och använda den aktuella datumet är en viktig del av programmering i C#. Det är en grundläggande funktion som hjälper till att skapa dynamiska program och hantera olika datumformat. Oavsett om det är för en kalenderapplikation eller för att spåra transaktioner i ett program, behöver man ofta veta det aktuella datumet.

## Hur man gör det

Det finns flera sätt att få det aktuella datumet i C#. Det enklaste sättet är att använda DateTime-klassen. Detta är en inbyggd typ i C# och ger information om datum och tid. För att få det aktuella datumet behöver du bara skapa en DateTime-variabel och tilldela den till metoden Now().

```csharp
DateTime datum = DateTime.Now;

Console.WriteLine(datum);
```

Med detta enkla kodavsnitt kommer du att få det aktuella datumet i det format som är inställt på din dator.

## Djupdykning

Om du vill ha mer kontroll över formatet på det aktuella datumet finns det flera sätt att göra det. Ett sätt är att använda ToString() metoden och ange ett format som önskas. Till exempel kan du använda "d" för ett kort datumformat eller "D" för ett långt datumformat.

```csharp
DateTime datum = DateTime.Now;

Console.WriteLine(datum.ToString("d")); // kommer att ge 03/10/2021
Console.WriteLine(datum.ToString("D")); // kommer att ge 03 oktober 2021
```

En annan användbar funktion är att kunna ändra tidszonen för det aktuella datumet. Detta kan göras genom att använda TimeZoneInfo-klassen och dess metoder.

```csharp
DateTime datum = DateTime.Now;

TimeZoneInfo tidszon = TimeZoneInfo.FindSystemTimeZoneById("Central European Standard Time"); // ändra till önskad tidszon
DateTime aktuelltDatum = TimeZoneInfo.ConvertTime(datum, tidszon);

Console.WriteLine(aktuelltDatum);
```

Detta kommer att ge det aktuella datumet i tidszonen som anges.

## Se också

- [DateTime Class](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime?view=net-5.0)
- [TimeZoneInfo Class](https://docs.microsoft.com/sv-se/dotnet/api/system.timezoneinfo?view=net-5.0)
- [C# Datetime formats](https://www.csharp-examples.net/string-format-datetime/)