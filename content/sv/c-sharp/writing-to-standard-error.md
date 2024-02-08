---
title:                "Skriva till standardfel"
aliases:
- sv/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:32:50.859456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) i C# innebär att rikta felmeddelanden och diagnostik separat från reguljär utdata (stdout) för att hjälpa användare och utvecklare att skilja mellan normal programutdata och felnotifikationer. Programmerare gör detta för att göra felsökning och loggning mer effektiv, vilket möjliggör smidigare drift och underhåll av applikationer.

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
