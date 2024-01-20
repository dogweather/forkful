---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Lua-kodning: Hur man kontrollerar om en mapp finns

## Vad och Varför?
Att kontrollera om en mapp finns innebär att se om en specifik mapp existerar på skivan. Programmerare gör detta för att undvika fel orsakade av att försöka komma åt en mapp som inte finns.

## Hur man gör:

Här är exempel på hur du kan kontrollera om en mapp finns i Lua.

```Lua
local lfs = require('lfs')

function directory_exists(path)
    if lfs.attributes(path, 'mode') == 'directory' then
        return true
    else
        return false
    end
end

print(directory_exists("/path/to/directory"))  -- Replace with your directory path
```
Om mappen finns, skriver koden ut `true`. Annars skriver den ut `false`.

## Djupdykning

Historiskt sett har Lua inte haft ett inbyggt sätt att kontrollera om en fil eller mapp finns. Därför behövs biblioteket LuaFileSystem (lfs). Det finns alternativ till lfs, som os.execute och io.open, men dessa har nackdelar. os.execute kan vara långsamt och osäkert, medan io.open inte kan skilja mellan filer och mappar.

Det är viktigt att notera att funktionen lfs.attributes i exemplet ovan returnerar en tabell med information om mappen om den finns. Elementet 'mode' i denna tabell indikerar om sökvägen är en mapp, en fil eller något annat.

## Se även

1. [LuaFileSystem dokumentation](http://keplerproject.github.io/luafilesystem/manual.html#attributes)
2. [Lua User Wiki: Fil och Mapp funktioner](http://lua-users.org/wiki/FileAndDirectoryOperations)