---
title:    "C#: Att läsa inmatade kommandoradsargument"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument kan vara ett användbart verktyg för att effektivisera din kodning. Genom att läsa argument direkt från kommandoraden kan du anpassa programmet efter specifika behov utan att behöva ändra i källkoden.

## Så här gör du

För att läsa kommandoradsargument i C#, behöver du först inkludera System namespace i din kod.

```C#
using System;
```

Sedan kan du använda "command line arguments" arrayen för att läsa in argumenten som skickas in till programmet vid körning.

```C#
string[] args = Environment.GetCommandLineArgs();
```

För att förstå hur detta fungerar, låt oss se på ett exempel.

Om du kör ett program från kommandoraden som:

`> myprogram.exe argument1 argument2`

så kommer `args` arrayen att innehålla `["myprogram.exe", "argument1", "argument2"]`.

Du kan då använda indexering för att läsa in de specifika argumenten som du behöver. T.ex. `args[1]` för att läsa in "argument1".

Det är även möjligt att loopa igenom alla argument och göra olika åtgärder beroende på vad du vill uppnå.

Nu när du vet hur du läser kommandoradsargument, kan du lätt anpassa ditt program och göra det mer användarvänligt.

## Djupdykning

Vill du veta mer om hur kommandoradsargument fungerar i C#? Då kan du kolla in dokumentationen för Environment.GetCommandLineArgs() för mer detaljerad information. Det finns även andra sätt att läsa argument, såsom att använda en parser som CommandLineParser eller getopt, som ger mer avancerade möjligheter för att hantera argumenten.

## Se även

- [Environment.GetCommandLineArgs() dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [CommandLineParser](https://github.com/commandlineparser/commandline)
- [getopt](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)