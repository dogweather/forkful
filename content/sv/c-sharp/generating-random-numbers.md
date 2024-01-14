---
title:    "C#: Generera slumpmässiga nummer"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig uppgift inom programmering, särskilt inom spelutveckling, dataanalys och simuleringar. Det kan också vara användbart för att skapa unika identifierare eller lösenord.

## Så här gör du

Det finns flera sätt att generera slumpmässiga nummer i C#. En enkel metod är att använda klassen `Random` och dess metod `Next()` för att generera ett heltalsvärde som visas nedanför:

```C#
// Skapar en ny instans av klassen Random
Random slumpgenerator = new Random();

// Generera ett heltal i intervallet 1-100
int slumpmässigtNummer = slumpgenerator.Next(1, 101);

// Skriv ut det slumpmässiga numret
Console.WriteLine("Det slumpmässiga numret är: " + slumpmässigtNummer);
```

Output:
```
Det slumpmässiga numret är: 39
```

För att generera flera slumpmässiga nummer kan man sätta koden i en `for`-loop eller använda metoden `Next()` flera gånger. Det finns också andra metoder i klassen `Random` som kan generera andra datatyper som flyttal eller booleska värden.

## Djupdykning

Vad som kan vara viktigt att veta är att "slump" i datorvärlden egentligen inte är helt slumpmässigt, utan baseras på en algoritm som genererar ett mönster som vi människor uppfattar som slumpmässigt. Det finns också olika algoritmer som kan användas för att skapa olika typer av slumpmässiga nummer, beroende på vad det ska användas till.

Det finns också sätt att förbättra slumpmässigheten, som att använda systemtid eller unika värden som frö för algoritmen. Det är också viktigt att vara medveten om att en dator inte kan skapa verkligt slumpmässiga nummer, utan det är alltid baserat på någon form av algoritm.

## Se även

- [Dokumentation för klassen Random i .NET](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- [Random Number Generation in C# (Programiz)](https://www.programiz.com/csharp-programming/random-number)
- [The Danger of Pseudo-Random Numbers (Medium)](https://medium.com/@afshinea/the-danger-of-pseudo-random-numbers-e29758ea38d2)