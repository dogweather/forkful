---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Från Datum till Sträng i C#: Ett Komplett Guide

## Vad & Varför?
Att konvertera ett datum till en sträng i programmering innebär att förändra datatypen av ett värde från en datumtyp till en strängtyp. Programmers gör detta för att underlätta presentation och manipulering av datumvärden.

## Hur man gör:
Vi kommer att använda DateFormat klass i .NET Framework för att konvertera datum till strängar. 

```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime today = DateTime.Now;
        string dateString = today.ToString("d");

        Console.WriteLine(dateString);
    }
}
```
Kör den här koden och den kommer att skriva ut dagens datum i formatet mm/dd/yyyy.

## Fördjupning
Det är alltid bra att veta lite historia bakom sådana operationer. Funktionen att konvertera datum till strängar introducerades först i tidiga versioner av .NET Framework för att tillgodose behovet att presentera datumvärden på ett format som är lättförståeligt för människor.

Alternativt kan DateTime.ToString (string) eller DateTime.ToString (string, IFormatProvider) användas om du vill specificera ett anpassat format eller kulturinformation.

Implementeringsdetaljerna bakom ModelRenderer-klassen innebär att varje datumformat har en motsvarande formatsträng. Den här strängen passeras till ToString-metoden för att bestämma hur det resulterande strängdatumet ser ut.

## Se Även
För mer information, besök följande länkar:

1. Dokumentation för .NET Framework's DateTime [här](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0).
2. Information om anpassade datum- och tidsformatsträngar [här](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).