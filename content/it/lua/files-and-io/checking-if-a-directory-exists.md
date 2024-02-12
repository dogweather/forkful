---
title:                "Verifica se una directory esiste"
aliases:
- /it/lua/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:50.968236-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Controllare se una directory esiste è un'operazione fondamentale quando si scrivono script che interagiscono con il filesystem, garantendo che il proprio programma operi su percorsi validi e prevenga errori legati a directory inesistenti. Questo compito è cruciale per creare nuovi file nelle directory, leggerli o eseguire operazioni specifiche della directory in modo sicuro.

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
