---
title:                "Läsa kommandoradsargument"
html_title:           "C#: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa in argument från kommandoraden är en viktig kunskap för alla C#-programmerare. Det ger oss möjlighet att göra våra program mer anpassningsbara och effektiva genom att kunna ta emot olika data från användaren. Det är också ett grunderna ämne inom programmering och kommer att nyttigt oavsett vad man vill utveckla.

## Så här gör du

För att läsa in argument från kommandoraden i C#, kan vi använda "args" parameteren i funktionen "Main". Detta är en array som innehåller alla argument som angivits när programmet startas. Se nedan för ett exempel:

```C#
static void Main(string[] args)
{
    Console.WriteLine("Argument som angavs: ");
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

Om vi till exempel anger "dotnet program.exe Hej världen!", kommer konsolen att skriva ut:

Argument som angavs:
Hej
världen!

Vi kan också använda metoden "Environment.GetCommandLineArgs" för att få åtkomst till argumenten från vilken som helst del av programmet. Se följande exempel:

```C#
static void Main(string[] args)
{
    string[] commandLineArgs = Environment.GetCommandLineArgs();
    Console.WriteLine("Argument som angavs: ");
    foreach (string arg in commandLineArgs)
    {
        Console.WriteLine(arg);
    }
}
```

Det finns också möjlighet att använda externa paket som tillhandahåller mer avancerade funktioner för att hantera och tolka argument från kommandoraden.

## Fördjupning

När vi läser in argument från kommandoraden, är det viktigt att vi förstår de olika delarna av argumenten och hur de ska tolkas. Argument kan till exempel vara flaggor som indikerar ett specifikt beteende eller de kan vara datavärden som användaren vill skicka till programmet. Det är också viktigt att vi hanterar eventuella felaktiga argument eller om användaren inte angav några argument alls.

För att lära dig mer om hur du hanterar och tolkar argument från kommandoraden i C#, kan du läsa dokumentationen för ditt val av externa paket eller kolla på olika exempel och övningar online.

## Se också

- Microsoft dokumentation: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments
- Tutorial: https://www.tutorialspoint.com/csharp/csharp_command_line_arguments.htm
- Youtube-video: https://www.youtube.com/watch?v=2XAmk8QGJNs