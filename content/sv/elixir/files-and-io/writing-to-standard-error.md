---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:04.629717-07:00
description: "Hur: I Elixir kan du anv\xE4nda funktioner i `IO`-modulen s\xE5som `IO.puts/2`\
  \ och `IO.warn/2` f\xF6r att skriva meddelanden till standardfel."
lastmod: '2024-03-13T22:44:37.583917-06:00'
model: gpt-4-0125-preview
summary: "I Elixir kan du anv\xE4nda funktioner i `IO`-modulen s\xE5som `IO.puts/2`\
  \ och `IO.warn/2` f\xF6r att skriva meddelanden till standardfel."
title: Skriva till standardfel
weight: 25
---

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
