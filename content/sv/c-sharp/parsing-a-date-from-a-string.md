---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att parsa ett datum från en sträng innebär att omvandla en textrepresention av ett datum till datumobjekt som programmeraren kan använda. Programutvecklare gör detta för att hantera data från olika källor där formatet inte alltid är konsekvent.

## Hur man gör:
Här är en bit kod som visar hur du parsa ett datum från en sträng i C#:
```C#
using System;

class Program
{
    static void Main()
    {
        string dateString = "2020-08-20";
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate);
    }
}
```
Om du kör koden kommer du att se följande output:
```
2020-08-20 00:00:00
```
## Djupgående
(1) Historiskt sett, har konvertering av strängar till datum alltid varit ett behov i programmering, särskilt när man hanterar användarinmatningar eller data från olika källor. C# erbjuder flera metoder för att möta detta behov.

(2) Alternativt, kan man använda `DateTime.TryParse` metoden, som returnerar en boolesk värde som indikerar om konverteringen lyckades eller inte.

(3) Metoden `DateTime.Parse` hanterar strängen som ett argument och försöker konvertera den till ett DateTime-objekt. Om strängen inte kan tolkas som ett datum, kommer programmet att generera ett `FormatException` fel.

## Se också
- Microsoft Docs: DateTime.Parse Method ([Engelska](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0))
- Microsoft Docs: DateTime.TryParse Method ([Engelska](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=net-5.0))
- Stack Overflow: How to convert a string to datetime in C#? ([Engelska](https://stackoverflow.com/questions/919244/how-to-convert-a-string-to-datetime-in-c-sharp))