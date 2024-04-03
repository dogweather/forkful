---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:53.562527-07:00
description: "Att kontrollera om en katalog finns \xE4r en grundl\xE4ggande operation\
  \ n\xE4r man skriver skript som interagerar med filsystemet, f\xF6r att s\xE4kerst\xE4\
  lla att ditt\u2026"
lastmod: '2024-03-13T22:44:38.053302-06:00'
model: gpt-4-0125-preview
summary: "Att kontrollera om en katalog finns \xE4r en grundl\xE4ggande operation\
  \ n\xE4r man skriver skript som interagerar med filsystemet, f\xF6r att s\xE4kerst\xE4\
  lla att ditt program arbetar med giltiga s\xF6kv\xE4gar och f\xF6rhindrar fel relaterade\
  \ till icke-existerande kataloger."
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
