---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:04.629717-07:00
description: "Att skriva till standardfel (stderr) i Elixir \xE4r en metod f\xF6r\
  \ att dirigera felmeddelanden och diagnostik separat fr\xE5n huvudutdata (stdout).\
  \ Programmerare\u2026"
lastmod: '2024-03-11T00:14:10.915245-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Elixir \xE4r en metod f\xF6r att\
  \ dirigera felmeddelanden och diagnostik separat fr\xE5n huvudutdata (stdout). Programmerare\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) i Elixir är en metod för att dirigera felmeddelanden och diagnostik separat från huvudutdata (stdout). Programmerare använder stderr för att felsöka och hantera fel utan att göra programmets huvudutdata oöverskådlig, vilket gör det enklare att identifiera och åtgärda problem.

## Hur:

I Elixir kan du använda funktioner i `IO`-modulen såsom `IO.puts/2` och `IO.warn/2` för att skriva meddelanden till standardfel:

```elixir
# Skriva ett enkelt meddelande till stderr
IO.puts(:stderr, "Fel: Något gick fel!")

# Använda IO.warn, vilket är mer semantiskt för varningar/fel
IO.warn("Varning: Du är på väg att överskrida gränsen!")
```

Exempel på utdata i terminalen för `IO.puts/2`:
```
Fel: Något gick fel!
```

För `IO.warn/2` skulle utdatan vara liknande, men `IO.warn/2` är specifikt utformat för varningar och kan inkludera ytterligare formatering eller beteende i framtida Elixir-versioner.

**Använda Tredjepartsbibliotek**

Även om Elixirs standardbibliotek vanligtvis är tillräckligt för att hantera utdata till standardfel, kan du finna bibliotek som `Logger` användbara för mer komplexa applikationer eller för att konfigurera olika loggnivåer och utdatamy.

Exempel på användning av `Logger` för att utge ett felmeddelande:

```elixir
require Logger

# Konfigurera Logger för att utdata till stderr
Logger.configure_backend(:console, device: :stderr)

# Skriva ett felmeddelande
Logger.error("Fel: Misslyckades med att ansluta till databasen.")
```

Denna inställning dirigerar `Logger`-s utdata specifikt till stderr, vilket är användbart för att separera felloggning från standard loggmeddelanden.
