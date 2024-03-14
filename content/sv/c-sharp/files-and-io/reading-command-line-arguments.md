---
date: 2024-01-20 17:55:43.214102-07:00
description: "Att l\xE4sa kommandoradsargument inneb\xE4r att man h\xE4mtar de v\xE4\
  rden som anv\xE4ndaren har angett n\xE4r de startar programmet. Programmerare anv\xE4\
  nder det f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.928810-06:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa kommandoradsargument inneb\xE4r att man h\xE4mtar de v\xE4rden\
  \ som anv\xE4ndaren har angett n\xE4r de startar programmet. Programmerare anv\xE4\
  nder det f\xF6r att\u2026"
title: "L\xE4sa in kommandoradsargument"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument innebär att man hämtar de värden som användaren har angett när de startar programmet. Programmerare använder det för att tillåta användarinteraktion och för att göra program flexibla genom att ta emot input som styr programmets beteende direkt från start.

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
