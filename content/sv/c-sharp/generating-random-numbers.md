---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:44.450638-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal i C# handlar om att skapa tal som inte är förutsägbara, för användning i allt från spel till säkerhetsapplikationer. Programmerare använder det för att tillföra oförutsägbarhet och för att simulera slumpmässiga händelser.

## Hur man gör:
```C#
using System;

public class RandomNumberExample
{
    public static void Main()
    {
        // Skapa en instans av Random-klassen
        Random random = new Random();
        
        // Skapa ett slumptal mellan 0 och 99
        int randomNumber = random.Next(100);
        Console.WriteLine(randomNumber);

        // Skapa ett slumptal mellan 10 och 19
        int anotherRandomNumber = random.Next(10, 20);
        Console.WriteLine(anotherRandomNumber);
    }
}
```
### Exempel på output:
```
42
15
```
Notera att varje gång programmet körs, kan output variera.

## Fördjupning:
Historiskt har slumptalsgeneratorer varit svåra att göra riktigt bra. Datorer älskar ordning, så att skapa äkta "slump" är inte trivialt. .NET Framework erbjuder `System.Random` som en enkel lösning för de flesta behov. Den använder en pseudoslumptalsgenerator (PRNG), som ger en sekvens av tal som bara ser slumpmässiga ut.

Det finns även kryptografiskt säkra alternativ, som `System.Security.Cryptography.RandomNumberGenerator`. Denna är långsammare men mycket säkrare och används när man behöver hög säkerhet, som i lösenordsskapare eller säkerhetskrypteringar.

Implementationen av PRNG i C# bygger på en matematisk formel som genererar en sekvens baserad på ett startvärde, kallat ett frö (engelska "seed"). Fröet avgör hela sekvensens ordning, så om du använder samma frö får du samma sekvens.

## Se även:
- Microsofts dokumentation om `Random` klassen: [Random Class (System)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- Artikel om kryptografisk säkerhet för slumptalsgeneratorer: [Cryptographic Randomness](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0)