---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:53.562527-07:00
description: "Hur man g\xF6r: I Lua har du ingen inbyggd funktion f\xF6r att direkt\
  \ kontrollera om en katalog finns, s\xE5 du f\xF6rlitar dig ofta p\xE5 Lua File\
  \ System (lfs)\u2026"
lastmod: '2024-03-13T22:44:38.053302-06:00'
model: gpt-4-0125-preview
summary: "I Lua har du ingen inbyggd funktion f\xF6r att direkt kontrollera om en\
  \ katalog finns, s\xE5 du f\xF6rlitar dig ofta p\xE5 Lua File System (lfs) biblioteket,\
  \ ett popul\xE4rt tredjepartsbibliotek f\xF6r filoperationer."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur man gör:
I Lua har du ingen inbyggd funktion för att direkt kontrollera om en katalog finns, så du förlitar dig ofta på Lua File System (lfs) biblioteket, ett populärt tredjepartsbibliotek för filoperationer.

Se till att du har Lua File System installerat först. Om inte, kan du vanligtvis installera det med LuaRocks:

```sh
luarocks install luafilesystem
```

Sedan kan du använda följande exempel för att kontrollera om en katalog finns:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- Kontrollera om en specifik katalog finns
if directoryExists("/path/to/your/directory") then
    print("Katalogen finns.")
else
    print("Katalogen finns inte.")
end
```

Detta kommer att ge utskriften:

```
Katalogen finns.
```

Eller, om katalogen inte finns:

```
Katalogen finns inte.
```

Detta tillvägagångssätt använder funktionen `lfs.attributes` för att få attributen för sökvägen. Om sökvägen finns och dess `mode`-attribut är `directory`, bekräftar det katalogens existens.
