---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:20.631255-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Vi pratar om `debug output` – textmeddelanden som hjälper utvecklare att felsöka kod. Varför? För att spåra vad koden gör och hitta buggarna snabbt.

## How to:
Med `Console.WriteLine`, `Debug.WriteLine` eller `Trace.WriteLine` kan du skriva ut meddelanden. Använd `Console.WriteLine` för konsollappar och de andra när du behöver mer flexibilitet.

```C#
using System;
using System.Diagnostics;

public class DebugDemo
{
    public static void Main()
    {
        Console.WriteLine("Console output: Här körs programmet!");
        Debug.WriteLine("Debug output: Nu har du träffat en checkpunkt.");
        Trace.WriteLine("Trace output: Följer flödet här.");
    }
}
```

Om du kör konsollprogrammet får du nåt liknande:
```
Console output: Här körs programmet!
```

Notera att `Debug` och `Trace` output visas bara i Debug-fönstret i utvecklingsmiljön.

## Deep Dive:
Förr använde folk metoder som `printf` för C och C++. Nu har vi integrerade utvecklingsmiljöer (IDE) och avancerade verktyg som ger oss `Debug` och `Trace` klasserna i .NET.

Alternativ? Loggningsramverk som `NLog` eller `log4net`, speciellt för större applikationer.

Implementation? `Debug` och `Trace` använder lyssnare som fångar och hanterar utskrifter. Utan rätt konfiguration, ingen output.

## See Also:
- [Microsoft's Debug Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-6.0)
- [Microsoft's Trace Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace?view=net-6.0)
- [Intro to Logging in .NET](https://docs.microsoft.com/en-us/dotnet/core/extensions/logging?tabs=command-line)
