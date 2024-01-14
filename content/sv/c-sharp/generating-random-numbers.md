---
title:    "C#: Generering av slumpmässiga nummer"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Generering av slumpmässiga nummer är en viktig del av programmering eftersom det låter oss skapa spel, utföra statistiska analyser och utföra andra kritiska uppgifter. Genom att använda slumpmässiga nummer kan vi skapa program som är mer realistiska och tillförlitliga.

## Hur man gör

För att generera slumpmässiga nummer i C# kan vi använda oss av "Random" klassen. Här är ett enkelt exempel på hur detta kan göras:

```C#
Random random = new Random(); // Skapar en ny Random objekt
int randomNumber = random.Next(1, 11); // Genererar ett slumpmässigt nummer mellan 1 och 10

Console.WriteLine($"Slumpmässigt nummer: {randomNumber}"); // Skriver ut det slumpmässiga numret
```

När koden körs kommer ett slumpmässigt nummer mellan 1 och 10 att genereras och skrivas ut på skärmen. Detta är bara en enkel användning av "Random" klassen och det finns flera andra metoder och funktioner som kan användas för att skapa olika typer av slumpmässiga nummer.

## Djupdykning

Slumpmässiga nummer genereras med hjälp av en algoritm som kallas för "pseudo-random number generator (PRNG)". Detta innebär att numren som genereras inte är helt slumpmässiga utan beräknade baserat på en startpunkt, kallad "seed". Om vi exempelvis använde samma "seed" i vårt första exempel skulle det alltid generera samma sekvens av nummer.

En annan viktig faktor att tänka på är att datorer inte är kapabla att generera riktigt slumpmässiga nummer, eftersom de är byggda för att följa instruktioner. Istället använder de olika algoritmer för att simulera slumpmässighet.

## Se även

- [C# Random Klass](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [Slumpmässiga nummer i C#](https://www.geeksforgeeks.org/random-number-generator-in-c-sharp/)
- [Pseudo-random number generator](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)