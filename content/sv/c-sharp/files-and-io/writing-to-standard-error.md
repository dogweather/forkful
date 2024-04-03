---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:50.859456-07:00
description: "Hur man g\xF6r: I C# kan skrivning till standardfel uppn\xE5s genom\
  \ att anv\xE4nda str\xF6mmen `Console.Error`. Denna str\xF6m anv\xE4nds specifikt\
  \ f\xF6r felmeddelanden och\u2026"
lastmod: '2024-03-13T22:44:37.929801-06:00'
model: gpt-4-0125-preview
summary: "I C# kan skrivning till standardfel uppn\xE5s genom att anv\xE4nda str\xF6\
  mmen `Console.Error`."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I C# kan skrivning till standardfel uppnås genom att använda strömmen `Console.Error`. Denna ström används specifikt för felmeddelanden och diagnostik. Här är ett grundläggande exempel:

```csharp
Console.Error.WriteLine("Error: Misslyckades med att bearbeta förfrågan.");
```

Exempel på utdata (till stderr):
```
Error: Misslyckades med att bearbeta förfrågan.
```

För scenarier där du kanske använder ett tredjepartsbibliotek som erbjuder avancerade loggningsmöjligheter, som `Serilog` eller `NLog`, kan du konfigurera dessa bibliotek för att skriva felloggar till stderr. Även om dessa exempel fokuserar på enkel konsolomdirigering, kom ihåg att i produktionsapplikationer erbjuder loggningsramverk mycket mer robust felhantering och utdatalternativ. Här är ett enkelt exempel med `Serilog`:

Först, installera Serilog-paketet och dess Console sink:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Sedan, konfigurera Serilog för att skriva till stderr:

```csharp
using Serilog;

Log.Logger = en ny LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Det här är ett vanligt meddelande.");
Log.Error("Det här är ett felmeddelande.");
```

Exempel på utdata (till stderr för felmeddelandet):
```
[15:04:20 ERR] Det här är ett felmeddelande.
```

Notera: Konfigurationen `standardErrorFromLevel` i Serilogs console sink omdirigerar alla logghändelser på den angivna nivån (Error, i detta fall) eller högre till standardfelsströmmen, medan meddelanden på lägre nivåer som Information skrivs till standardutströmmen.
