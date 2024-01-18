---
title:                "Stora bokstäver i en sträng"
html_title:           "Lua: Stora bokstäver i en sträng"
simple_title:         "Stora bokstäver i en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "kapsla in" en sträng innebär att göra om den första bokstaven i varje ord till en stor bokstav. Programmerare gör detta ofta för att göra texten mer läsbar och för att följa allmänna kodningsstilar.

## Hur?
För att kapsla in en sträng i Lua, använd funktionen "string.upper". Nedan är ett exempel där vi förvandlar en sträng till alla stora bokstäver:

```Lua
local str = "denna sträng kommer att förvandlas"
print(string.upper(str))
```

Detta skulle ge oss följande utmatning:
`DENNA STRÄNG KOMMER ATT FÖRVANDLAS`

## Djupdykning
Historiskt sett har kapsling av strängar använts för att förbättra läsbarheten av text i olika språk. Det finns också alternativ till att använda "string.upper" funktionen, som till exempel att använda nyckelordet "local" för att definiera en variabel. Implementeringsdetaljer för denna funktion kan variera beroende på språket som används.

## Se också
Om du vill lära dig mer om strängar och andra grundläggande koncept i Lua, se följande källor:
- [Lua.org](https://www.lua.org/) – Officiell hemsida för Lua med dokumentation och resurser.
- [Lua-tutorial.org](http://lua-tutorial.org/) – En nybörjarvänlig handledning för Lua-programmering.
- [Learn Lua in 15 Minutes](https://learnxinyminutes.com/docs/lua/) – En snabb översikt av Lua:s syntax och funktioner.