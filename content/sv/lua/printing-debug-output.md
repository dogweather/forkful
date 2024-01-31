---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:50.915365-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Skriva ut felsökningsmeddelanden i Lua hjälper dig att förstå vad som händer i koden. Vi gör det för att snabbt identifiera och åtgärda buggar.

## How to:
Använd `print()` för att visa värden. Kolla exemplet:

```Lua
local variabel = "Hej"
print(variabel)  -- Skriver ut: Hej

-- För mer komplexa typer kan du använda pairs() för att iterera:
local tabell = {nyckel1 = "värde1", nyckel2 = "värde2"}
for nyckel, värde in pairs(tabell) do
    print(nyckel, värde)
end
-- Skriver ut:
-- nyckel1      värde1
-- nyckel2      värde2
```

## Deep Dive:
Felsökning genom utskrift har funnits sedan programmeringens gryning. Lua är inget undantag. Varför `print()`? Det är enkelt. Men det finns alternativ, till exempel `io.write()`, som ger mer kontroll över formatet. `print()` lägger automatiskt till en radbrytning, `io.write()` gör inte det.

I komplexa program, där prestanda och struktur är viktiga, kan `print()` bli rörigt. Då kan inbyggda eller tredjeparts loggbibliotek som `LuaLogging` vara bättre. De erbjuder loggnivåer (INFO, ERROR, etc.), filutskrift och formaterade meddelanden.

## See Also:
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
- LuaLogging på GitHub: https://github.com/lunarmodules/lualogging
