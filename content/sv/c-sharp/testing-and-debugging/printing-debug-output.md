---
date: 2024-01-20 17:52:20.631255-07:00
description: "How to: Med `Console.WriteLine`, `Debug.WriteLine` eller `Trace.WriteLine`\
  \ kan du skriva ut meddelanden. Anv\xE4nd `Console.WriteLine` f\xF6r konsollappar\
  \ och\u2026"
lastmod: '2024-03-13T22:44:37.915824-06:00'
model: gpt-4-1106-preview
summary: Med `Console.WriteLine`, `Debug.WriteLine` eller `Trace.WriteLine` kan du
  skriva ut meddelanden.
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

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
