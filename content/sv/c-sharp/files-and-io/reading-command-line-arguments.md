---
date: 2024-01-20 17:55:43.214102-07:00
description: "S\xE5 h\xE4r g\xF6r du: Anv\xE4nd `args` i din `Main` metod f\xF6r att\
  \ ta emot argument. H\xE4r \xE4r ett exempel p\xE5 hur det kan se ut."
lastmod: '2024-03-13T22:44:37.928810-06:00'
model: gpt-4-1106-preview
summary: "Anv\xE4nd `args` i din `Main` metod f\xF6r att ta emot argument."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Så här gör du:
Använd `args` i din `Main` metod för att ta emot argument. Här är ett exempel på hur det kan se ut:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length > 0)
        {
            Console.WriteLine("Hej! Du angav följande argument:");
            foreach (var arg in args)
            {
                Console.WriteLine(arg);
            }
        }
        else
        {
            Console.WriteLine("Inga argument angivna.");
        }
    }
}
```
Om du skulle köra programmets exe-fil från kommandotolken så här: `program.exe Hej Tjena Hallå`, skulle utskriften bli:
```
Hej! Du angav följande argument:
Hej
Tjena
Hallå
```

## Fördjupning:
Att läsa kommandoradsargument är grundläggande och har funnits sedan de tidiga dagarna av programmering. Alternativ till kommandoradsargument inkluderar interaktion genom grafiska användargränssnitt eller konfigurationsfiler.

Det som är viktigt att veta är att `args` i `Main` är en array av strängar. Argumenten delas upp baserat på blanksteg, så om ett argument innehåller ett blanksteg behöver det omslutas av citattecken när det skickas in.

För komplexa behov finns bibliotek som `CommandLineParser` som hanterar mer avancerade scenarion, t.ex. flaggor och optioner.

## Se även:
- Microsofts dokumentation om kommandoradsargument: https://docs.microsoft.com/dotnet/csharp/programming-guide/main-and-command-args/
- CommandLineParser biblioteket på NuGet: https://www.nuget.org/packages/CommandLineParser/
- Microsofts .NET Guide om hur man arbetar med kommandorad: https://docs.microsoft.com/dotnet/core/extensions/command-line-args
