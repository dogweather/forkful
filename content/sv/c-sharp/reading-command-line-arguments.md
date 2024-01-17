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

## Vad & Varför?
Läsning av kommandoradsargument är en vanlig praxis bland programmerare för att få åtkomst till information som användaren skriver in vid körning av ett program. Genom att läsa in dessa argument kan programmeraren göra sitt program mer användarvänligt och addera olika funktioner baserat på användarens input.

## Hur man gör:
```C#
// Kodexempel för läsning av kommandoradsargument:
static void Main(string[] args)
{
    // Läs in argumentslista från kommandoraden
    string[] arguments = Environment.GetCommandLineArgs();

    // Loopa igenom arguments och skriv ut dem
    foreach (string argument in arguments)
    {
        Console.WriteLine(argument);
    }
}
```
Exempel på hur programmet kan köras från kommandoraden och dess output:
```
> myProgram.exe arg1 arg2
arg1
arg2
```

## Djupdykning:
Historiskt sett har kommandoradsargument varit en vanlig metod för att konfigurera och köra program på operativsystem som DOS och Unix. Idag används det fortfarande flitigt även i modernt operativsystem som Windows och macOS.

Det finns dock alternativ till kommandoradsargument, som till exempel att använda konfigurationsfiler eller grafiska användargränssnitt (GUI). Valet av metod beror oftast på programmerarens personliga preferenser och vilket som passar bäst för programmet i fråga.

Implementeringsdetaljer för läsning av kommandoradsargument kan skilja sig mellan olika programmeringsspråk, men principen är densamma. Det är viktigt att hantera eventuella fel och ogiltiga argument på ett säkert sätt för att undvika programkrascher.

## Se även:
- [Dokumentation för Environment.GetCommandLineArgs metoden (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Kommandoradsargument (Wikipedia på svenska)](https://sv.wikipedia.org/wiki/Kommandoradsargument)