---
title:                "Skriva till standardfel"
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standard error ("stderr") är att dirigera felmeddelanden och annan diagnostisk information till en separat kanal som standard. Det gör det enklare att skilja normalt utdata från felrapporter och att hantera loggar effektivare.

## Hur gör man:
Att skriva till stderr i Lua är enkelt. Använd `io.stderr:write()` för att skicka text till felströmmen:

```Lua
-- Skriver till standard error
io.stderr:write("Det här är ett felmeddelande!\n")
```

Om du kör det här skriptet är utskriften inte på stdout utan på stderr, vilket du märker om du omdirigerar det i en terminal:

```Bash
lua dittskript.lua 2> error.log
```

`error.log` kommer att innehålla "Det här är ett felmeddelande!"

## Djupdykning:
Historiskt har felhantering och loggning implementerats på många sätt, men konventionen att separera normal utdata (stdout) från fel (stderr) har varit standard sedan Unix-tiden. Alternativ till `io.stderr:write()` inkluderar att använda ett loggningsbibliotek eller skriva till filer. Både filer och stderr är strömmar i Lua, men skrivning till stderr verkställs direkt och kan inte buffras, vilket är kritiskt för att rapportera fel i realtid.

## Se också:
- Lua's `io` biblioteket: [https://www.lua.org/manual/5.4/manual.html#6.8](https://www.lua.org/manual/5.4/manual.html#6.8)
- Mer om felhantering i Lua: [https://www.lua.org/pil/8.4.html](https://www.lua.org/pil/8.4.html)
- Unix filsystemets standardströmmar: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)