---
aliases:
- /sv/lua/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:53.562527-07:00
description: "Att kontrollera om en katalog finns \xE4r en grundl\xE4ggande operation\
  \ n\xE4r man skriver skript som interagerar med filsystemet, f\xF6r att s\xE4kerst\xE4\
  lla att ditt\u2026"
lastmod: 2024-02-18 23:08:51.932541
model: gpt-4-0125-preview
summary: "Att kontrollera om en katalog finns \xE4r en grundl\xE4ggande operation\
  \ n\xE4r man skriver skript som interagerar med filsystemet, f\xF6r att s\xE4kerst\xE4\
  lla att ditt\u2026"
title: Kontrollera om en katalog existerar
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog finns är en grundläggande operation när man skriver skript som interagerar med filsystemet, för att säkerställa att ditt program arbetar med giltiga sökvägar och förhindrar fel relaterade till icke-existerande kataloger. Denna uppgift är avgörande för att skapa nya filer i kataloger, läsa från dem eller utföra katalogspecifika operationer på ett säkert sätt.

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
