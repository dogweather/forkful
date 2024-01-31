---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:57:31.102290-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Verificare l'esistenza di una directory è come sbirciare in una stanza per assicurarsi che ci sia qualcuno. I programmatori lo fanno per evitare errori come cercare file in un posto che non c'è.

## Come fare:
Per verificare se una directory esiste in Lua, usiamo la funzione `os.execute` o la libreria `lfs` (LuaFileSystem). Vediamo come:

```Lua
local lfs = require('lfs')

-- Usando LuaFileSystem
function directory_exists(path)
    return lfs.attributes(path, "mode") == "directory"
end

-- Verifica e stampa il risultato
if directory_exists("il_mio_percorso") then
    print("Yep, la directory esiste!")
else
    print("Ops, la directory non esiste.")
end
```

Semplice, vero? Se non hai `lfs`, installalo con `luarocks install luafilesystem`.

## Deep Dive
Prima di `lfs`, dovevi affidarti ai comandi specifici del sistema operativo. Inoltre, Lua non ha una funzione built-in per questo. Oltre a `lfs`, puoi utilizzare `os.execute` con comandi di sistema, ma attenzione: cambia da un OS all'altro. Questo controllo è cruciale quando si manipolano file, per non finire a scrivere in luoghi inesistenti.

Ecco un esempio con `os.execute`:

```Lua
function directory_exists(path)
    local cd_command = string.format("cd %s 2> /dev/null", path)
    if os.execute(cd_command) then
        return true
    else
        return false
    end
end
```

Attenzione: `os.execute` ha comportamenti diversi su sistemi diversi, quindi è più affidabile `lfs`.

## See Also
- Documentazione di LuaFileSystem: http://keplerproject.github.io/luafilesystem/
- LuaRocks, il gestore di pacchetti per Lua: https://luarocks.org/
- Guida sulla manipolazione dei file in Lua: https://www.lua.org/pil/21.1.html
