---
title:                "Generera slumptal"
aliases:
- /sv/c-sharp/generating-random-numbers/
date:                  2024-01-27T20:33:08.568637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumptal i C# innebär skapandet av oförutsägbara numeriska värden inom ett angivet intervall. Programmerare använder dessa metoder för att implementera funktioner som kryptografi, simuleringar och spel där oförutsägbarhet eller simulering av verklig slumpmässighet krävs.

## Hur man gör:

Det vanligaste sättet att generera slumptal i C# är att använda klassen `System.Random`. Här är ett enkelt exempel som demonstrerar dess användning:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Genererar ett nummer mellan 1 och 99
        Console.WriteLine($"Slumptal: {randomNumber}");
    }
}
```

Det här kommer att skriva ut ett slumptal som:

```
Slumptal: 42
```

För att generera ett slumpmässigt flyttal mellan 0,0 och 1,0 kan du använda metoden `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Slumpmässigt dubbel: {randomDouble}");
```

Om du arbetar med en säkerhetskänslig applikation som kräver kryptografisk slumpmässighet är det bättre att använda klassen `RNGCryptoServiceProvider` som finns i `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Skapar ett 4 byte långt slumptal
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Kryptografiskt säkert slumptal: {value}");
    }
}
```

## Djupdykning

Generering av slumptal i C# har utvecklats över åren. Inledningsvis var klassen `System.Random` det gå-tillvalet för att generera pseudoslumptal. Det är pseudoslumpmässigt eftersom det, med ett specifikt startvärde, kommer att producera samma sekvens av nummer, vilket kan vara användbart för felsökning eller upprepbarhet av tester.

Medan det är tillräckligt för grundläggande behov är `System.Random` inte trådsäkert och kan producera förutsägbara resultat, vilket inte är lämpligt för säkerhetsberoende applikationer. Denna begränsning ledde till införandet av `RNGCryptoServiceProvider` för kryptografisk slumpmässighet, vilket är säkrare men också mer resurskrävande.

Ett alternativ i .NET Core och .NET 5+ är klassen `RandomNumberGenerator` i `System.Security.Cryptography` för att generera slumptal på ett säkert sätt, vilket är tänkt som ett mer modernt och lättanvänt alternativ jämfört med `RNGCryptoServiceProvider`.

Varje metod för att generera slumptal i C# har sin plats beroende på tillämpningens krav. För de flesta applikationer räcker `System.Random`, men för de som kräver säkra, oförutsägbara slumptal erbjuder de kryptografiska klasserna ett robust alternativ.
