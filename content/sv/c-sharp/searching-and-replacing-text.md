---
title:    "C#: Sökning och ersättning av text"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför 

Att söka och ersätta text är en viktig färdighet för programmerare. Det låter dig enkelt byta ut en sträng av text med en annan i dina program eller dokument. Läs vidare för att lära dig mer om hur man gör detta i C#.

## Så här gör du

Sök och ersättning av text i C# är enkelt med hjälp av den inbyggda "Replace" funktionen. I exemplet nedan ersätter vi alla förekomster av "hej" med "hej hej" i en sträng.

```C#
string text = "Hej, jag heter Lisa. Hej Sara!";
string ersatt = text.Replace("hej", "hej hej");
Console.WriteLine(ersatt);
```

Output:

```
Hej hej, jag heter Lisa. Hej hej Sara!
```

Som du kan se ersattes både den första och andra förekomsten av "hej" med "hej hej". Detta fungerar också med Stor och Liten bokstav, så "hej" och "Hej" kommer att ersättas på samma sätt. 

## Djupdykning 

I Replace-funktionen kan du även ange ett tredje argument för att begränsa antalet förekomster som ersätts. Till exempel om vi bara vill ersätta det första "hej" med "hej hej" i vår tidigare sträng:

```C#
string text = "Hej, jag heter Lisa. Hej Sara!";
string ersatt = text.Replace("hej", "hej hej", 1);
Console.WriteLine(ersatt);
```

Output:

```
Hej hej, jag heter Lisa. Hej Sara!
```

Detta kan vara användbart när du vill byta ut en specifik del av en sträng och inte alla förekomster.

## Se även 

- [Microsoft Docs - String.Replace Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [W3Schools - C# Replace Method](https://www.w3schools.com/cs/cs_string_replace.asp)
- [C# Programming Guide - String.Replace Method](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#replacing-substrings-within-a-character-array)