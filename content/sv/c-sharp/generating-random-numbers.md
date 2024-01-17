---
title:                "Generering av slumpmässiga tal"
html_title:           "C#: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är en vanlig uppgift som programmerare måste hantera. Det hänvisar till att skapa ett nummer utan någon uppenbar logik eller ordning. Programmare gör detta för att skapa variation eller för att simulera slumpmässiga händelser, som i spel eller forskning.

## Hur man:
För att generera slumpmässiga nummer i C#, kan du använda Random-klassen. Här är ett exempel på kod som skapar fem slumpmässiga heltal mellan 1 och 10:

```C#
Random random = new Random();
for (int i = 0; i < 5; i++)
{
  int num = random.Next(1, 11);
  Console.WriteLine(num);
}
```
Exempel på utmatning:
```
6
3
8
1
10
```

## Djupdykning:
Generering av slumpmässiga nummer är en viktig del av datorspråk sedan början av datorerna. Tidiga metoder inkluderade användning av fysiska fenomen som elektronbrus eller radioaktivt sönderfall. Idag används mer avancerade algoritmer i programmeringsspråk som C#.

Alternativ till att använda Random-klassen är att använda en tredje parts bibliotek som erbjuder mer avancerade slumpgeneratorer. Dessutom finns det olika algoritmer och tekniker som programmerare kan använda för att skapa mer "slump" i sina nummer, som att använda faktorer såsom systemklockan eller användarens musrörelser.

## Se även:
https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1 - officiell dokumentation för Random-klassen i C#.

https://en.wikipedia.org/wiki/Random_number_generation - historisk och teknisk information om generering av slumpmässiga nummer.

https://www.baeldung.com/java-random - liknande artikel men för Java-programmeringsspråket.