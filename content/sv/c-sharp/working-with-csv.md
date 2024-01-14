---
title:                "C#: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV-filer är en vanlig typ av fil som används för att lagra och dela data. De kan enkelt skapas och läsas av både människor och datorer, vilket gör dem till ett populärt val för att hantera data. Om du är en programmerare eller datavetare, är det viktigt att veta hur man arbetar med CSV-filer för att kunna hantera och analysera data på ett effektivt sätt.

## How To

Att arbeta med CSV-filer i C# är en relativt enkel process. Du kan använda färdiga bibliotek som CSVHelper eller skriva din egen kod för att läsa och skriva till CSV-filer. Här är ett exempel på hur du kan läsa in data från en CSV-fil:

```C#
using (var reader = new StreamReader("minfil.csv"))
{
    using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
    {
        var records = csv.GetRecords<MinKlass>().ToList();
    }
}
```

I detta exempel används CSVHelper-biblioteket för att läsa in data från en CSV-fil och konvertera den till en lista av objekt av typen MinKlass. Här är en delvis skriven klass MinKlass som visar hur du kan definiera strukturen på data som läses in:

```C#
public class MinKlass
{
    public string Förnamn { get; set; }
    public string Efternamn { get; set; }
}
```

När du har läst in data från en CSV-fil, kan du enkelt utföra olika åtgärder på den, som att filtrera eller uppdatera värden. När du är klar med dina manipulationer kan du sedan spara data till en CSV-fil igen med hjälp av samma bibliotek eller egen kod.

## Deep Dive

Att arbeta med CSV-filer kan också innebära att hantera olika typer av datastrukturer, hantering av specialtecken eller att använda olika avgränsare (separatorer) för dina datafält. Om du är intresserad av att djupdyka i dessa koncept kan du läsa mer om CSV-formatet och dess specifikationer här: [https://tools.ietf.org/html/rfc4180] (https://tools.ietf.org/html/rfc4180)

## Se även

- [https://joshclose.github.io/CsvHelper/] (https://joshclose.github.io/CsvHelper/)
- [https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1] (https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1] (https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)