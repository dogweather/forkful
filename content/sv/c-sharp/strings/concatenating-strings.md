---
title:                "Sammanslagning av strängar"
aliases:
- /sv/c-sharp/concatenating-strings.md
date:                  2024-01-20T17:34:47.036014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammanslagning av strängar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Strängsammanslagning i C# innebär att slå ihop två eller flera textsträngar till en. Programmerare gör detta för att bygga dynamiska meddelanden, skapa SQL-queries, eller bara för att organisera informationen på ett snyggt sätt.

## Hur gör man?:
```C#
string hello = "Hej ";
string world = "världen!";
string combined = hello + world; // Använda + operatorn
Console.WriteLine(combined); // Output: Hej världen!

string greeting = String.Concat(hello, world); // Använda String.Concat
Console.WriteLine(greeting); // Output: Hej världen!

string formatted = $"{hello}{world}"; // Använda string interpolation
Console.WriteLine(formatted); // Output: Hej världen!
```

## Djupdykning:
Förr i tiden var sammanslagning av strängar i C# mindre effektivt, särskilt i lökker, på grund av hur strängar hanterades i minnet - varje sammanslagning skapade en ny sträng. Nu är `StringBuilder` klassen ett bra alternativ för att minska minnesanvändning och öka prestanda för stora eller många strängsammanslagningar.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hej ");
sb.Append("världen!");
string result = sb.ToString();
Console.WriteLine(result); // Output: Hej världen!
```

.NET ramverket har även optimerat `+` operatorn bakom kulisserna, men `StringBuilder` är fortfarande kung för komplexa scenarion.

Andra alternativ inkluderar `String.Format`, `String.Join`, och `String.Concat`. När man väljer metod beror det ofta på personlig preferens, situationens komplexitet eller prestandakrav.

## Se även:
- Microsofts officiella dokumentation om strängar: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/
- C# Guide - String Interpolation: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated
- Stack Overflow diskussion om strängsammanslagning: https://stackoverflow.com/questions/585860/string-concat-vs-stringbuilder
