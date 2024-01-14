---
title:                "C#: Att skriva till standardfel"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av C# programmering eftersom det ger oss möjlighet att hantera och spåra eventuella fel som kan uppstå under exekveringen av ett program. Genom att skriva till standardfel kan vi få värdefull information om vad som gått fel och var det har inträffat, vilket underlättar felsökningen och förbättrar kvaliteten på vårt kod.

## Hur man gör

För att skriva till standardfel i C# använder vi metoden `Console.Error.WriteLine()`. Detta gör att vi kan skriva ett meddelande till standardfelströmmen, vilket vanligtvis kopplas till en konsoll eller en loggfilsfil. Låt oss ta en titt på ett exempel:

```C#
try
{
    int x = 5;
    int y = 0;
    int result = x / y;
    Console.WriteLine($"Resultatet av divisionen är: {result}");
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Ett fel uppstod: {ex.Message}");
}
```

I detta exempel delar vi två heltal där nämnaren är noll, vilket resulterar i ett fel. Istället för att bara använda `Console.WriteLine()` för att skriva ut ett meddelande till standardutströmmen, använder vi `Console.Error.WriteLine()` för att skriva ut ett felmeddelande till standardfelströmmen. 

Output: 

`Ett fel uppstod: Divison med 0 är inte tillåtet.`

Genom att använda `Console.Error.WriteLine()` kan vi fånga och hantera eventuella fel och lämna användbara meddelanden för felsökning. Detta är en viktig del av att skriva robust och pålitlig kod.

## Deep Dive

I många fall kanske vi vill skriva till standardfelströmmen utan att nödvändigtvis fånga ett fel. Detta kan vara användbart om vi vill skriva ut varningar eller annan information om programmet som inte är kritiskt för dess exekveringshastighet. Det finns flera andra metoder i `Console.Error`-klassen som vi kan använda för att skriva till standardfelströmmen, som `Console.Error.Write()` för att skriva en sträng utan att lägga till en ny rad, eller `Console.Error.Flush()` för att tömma bufferten för standardfelströmmen.

Vi kan också använda `Console.SetError()`-metoden för att ändra standardfelström för vår applikation. Detta kan vara användbart om vi till exempel vill skriva felen till en loggfil istället för konsollen. 

## Se även

- [C# Dokumentation: Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error?view=net-5.0)
- [Using Standard Error Stream in C#](https://www.c-sharpcorner.com/article/using-standard-error-stream-in-c-sharp/)
- [Error Handling in C#](https://stackify.com/csharp-error-handling-best-practices/)