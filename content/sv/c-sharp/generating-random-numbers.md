---
title:                "C#: Generera slumpmässiga tal"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga tal kan vara en användbar funktion i många situationer inom programmering. Genom att använda sig av slumpmässiga tal blir programmet mer varierat och ger möjlighet till olika utfall. Det kan till exempel användas för att skapa slumpmässiga spelresultat, testa program med olika värden eller förbättra säkerheten med slumpmässiga krypteringsnycklar.

## Så här gör du

För att generera slumpmässiga tal i C# kan du använda dig av Random-klassen. Den här klassen ger funktioner för att generera både heltal och flyttal inom ett visst intervall. Nedan följer ett exempel på hur man kan använda Random-klassen för att generera 10 slumpmässiga tal mellan 1 och 100:

```C#
Random random = new Random(); // skapa ett nytt Random-objekt
for (int i = 0; i < 10; i++) // loopa 10 gånger
{
    int randomNumber = random.Next(1, 101); // generera ett heltal mellan 1 och 100
    Console.WriteLine(randomNumber); // skriv ut det slumpmässiga talet
}
```

Detta kommer att producera följande output:

```
74
16
97
5
89
26
55
33
48
91
```

Om du vill ha ett slumpmässigt flyttal istället för ett heltal kan du använda NextDouble-metoden istället för Next-metoden. Nästa avsnitt kommer att gå djupare in på hur detta fungerar.

## Djupdykning

Random-klassen i C# använder sig av en pseudoslumpgenerator för att generera slumpmässiga tal. Det betyder att talen inte är helt slumpmässiga utan följer ett matematiskt mönster. Det här kan vara viktigt att ha i åtanke när man använder sig av slumpmässiga tal för säkerhetsrelaterade uppgifter, eftersom det kan finnas möjlighet att förutsäga nästa tal.

Om du vill ha mer kontroll över de slumpmässiga talen kan du även använda dig av en specifik seed när du skapar ditt Random-objekt. En seed är ett värde som används för att starta den pseudoslumpgenerator som Random-klassen använder sig av. Om du använder samma seed får du alltid samma sekvens av slumpmässiga tal. Detta kan vara till nytta när du till exempel vill testa ett program med samma slumpmässiga tal flera gånger för att säkerställa dess pålitlighet.

## Se även

- [Random-klassen (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [Slumpgenerator (Wikipedia)](https://sv.wikipedia.org/wiki/Slumpgenerator)
- [Säkerhet av slumpmässiga tal (Huberts.net)](http://www.huberts.net/security/randomnumbers-rng/)