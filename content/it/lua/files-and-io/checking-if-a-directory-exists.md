---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:50.968236-07:00
description: "Come fare: In Lua, non esiste una funzione integrata per verificare\
  \ direttamente se una directory esiste, quindi spesso ci si affida alla libreria\
  \ Lua\u2026"
lastmod: '2024-03-13T22:44:43.571513-06:00'
model: gpt-4-0125-preview
summary: In Lua, non esiste una funzione integrata per verificare direttamente se
  una directory esiste, quindi spesso ci si affida alla libreria Lua File System (lfs),
  una popolare libreria di terze parti per le operazioni sui file.
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
In Lua, non esiste una funzione integrata per verificare direttamente se una directory esiste, quindi spesso ci si affida alla libreria Lua File System (lfs), una popolare libreria di terze parti per le operazioni sui file.

Prima di tutto, assicurati di avere Lua File System installato. In caso contrario, generalmente è possibile installarlo utilizzando LuaRocks:

```sh
luarocks install luafilesystem
```

Poi, puoi utilizzare il seguente esempio per verificare l'esistenza di una directory:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- Verifica se una directory specifica esiste
if directoryExists("/path/to/your/directory") then
    print("La directory esiste.")
else
    print("La directory non esiste.")
end
```

Questo produrrà in output:

```
La directory esiste.
```

Oppure, se la directory non esiste:

```
La directory non esiste.
```

Questo approccio utilizza la funzione `lfs.attributes` per ottenere gli attributi del percorso. Se il percorso esiste ed il suo attributo `mode` è `directory`, conferma l'esistenza della directory.
