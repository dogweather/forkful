---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:57:53.236234-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns innebär att man skriver kod som ser om en särskild katalog existerar på filsystemet. Programmerare gör detta för att undvika fel genom att förutsätta att en mapp finns, vilket kan vara fallet vid läsning, skrivning eller när man manipulerar filer.

## Hur man gör:
```Lua
local lfs = require("lfs") -- Ladda LuaFileSystem-modulen

-- Funktion för att kontrollera om en mapp finns
function directory_exists(path)
    local attributes = lfs.attributes(path)
    return attributes and attributes.mode == "directory"
end

-- Användning
local path = "/din/önskade/sökväg"
if directory_exists(path) then
    print("Mappen finns!")
else
    print("Mappen existerar inte.")
end
```
### Exempel på utdata:
```
Mappen finns!
```
eller
```
Mappen existerar inte.
```

## Djupdykning
Historiskt har tillgången till filsystemet i Lua krävt externa moduler som LuaFileSystem eftersom standardbiblioteket inte har den funktionaliteten. LuaFileSystem (lfs) är den vanligaste lösningen och har blivit de facto-standarden för filsystemåtgärder i Lua.

Alternativ till LuaFileSystem inkluderar att använda `os`-biblioteket för att köra kommandon som är specifika för operativsystemet, men detta är mindre portabelt och kan vara säkerhetsrisk. Det finns också andra tredjepartsbibliotek och FFI-biblioteket (Foreign Function Interface) som kan användas för att åstadkomma samma sak.

När det gäller implementationen använder `lfs.attributes` för att hämta attributen för en fil eller mapp. Genom att kontrollera att attributet 'mode' är 'directory' kan vi avgöra om sökvägen är en mapp.

## Se också
- LuaFileSystem dokumentation: http://keplerproject.github.io/luafilesystem/
- Lua 5.4 referensmanual: https://www.lua.org/manual/5.4/
- Lua Användar-Wiki för filsystemåtgärder: http://lua-users.org/wiki/FileSystemOperations