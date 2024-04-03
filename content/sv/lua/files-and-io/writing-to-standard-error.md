---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:50.608196-07:00
description: "Att skriva till standardfel (stderr) handlar om att rikta felmeddelanden\
  \ och diagnostisk utdata till en separat kanal, skild fr\xE5n standardutdata (stdout).\u2026"
lastmod: '2024-03-13T22:44:38.055251-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) handlar om att rikta felmeddelanden\
  \ och diagnostisk utdata till en separat kanal, skild fr\xE5n standardutdata (stdout)."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I Lua kan skrivning till stderr uppnås genom att använda funktionen `io.stderr:write()`. Så här kan du skriva ett enkelt felmeddelande till standardfel:

```lua
io.stderr:write("Fel: Ogiltig inmatning.\n")
```

Om du behöver mata ut en variabel eller kombinera flera datapunkter, konkateniera dem inom write-funktionen:

```lua
local felmeddelande = "Ogiltig inmatning."
io.stderr:write("Fel: " .. felmeddelande .. "\n")
```

**Exempelutdata på stderr:**
```
Fel: Ogiltig inmatning.
```

För mer komplexa scenarier, eller när man arbetar med större applikationer, kan man överväga tredjeparts loggningsbibliotek såsom LuaLogging. Med LuaLogging kan du rikta loggar till olika destinationer, inklusive stderr. Här är ett kort exempel:

Först, se till att LuaLogging är installerat med LuaRocks:

```
luarocks install lualogging
```

Sedan, för att skriva ett felmeddelande till stderr med LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Fel: Ogiltig inmatning.")
```

Detta tillvägagångssätt erbjuder fördelen av standardiserad loggning över hela din applikation, med tillagd flexibilitet att sätta loggningsnivåer (t.ex. ERROR, WARN, INFO) genom ett enkelt API.
