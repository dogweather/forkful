---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:46.940897-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis existiert, ist eine grundlegende\
  \ Operation, wenn man Skripte schreibt, die mit dem Dateisystem interagieren. Dabei\
  \ wird\u2026"
lastmod: '2024-03-13T22:44:54.029287-06:00'
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis existiert, ist eine grundlegende\
  \ Operation, wenn man Skripte schreibt, die mit dem Dateisystem interagieren."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie geht das:
In Lua gibt es keine eingebaute Funktion, um direkt zu überprüfen, ob ein Verzeichnis existiert. Daher greift man oft auf die Lua File System (lfs) Bibliothek zurück, eine beliebte Drittanbieterbibliothek für Dateioperationen.

Stellen Sie zunächst sicher, dass Sie Lua File System installiert haben. Falls nicht, können Sie es im Allgemeinen mit LuaRocks installieren:

```sh
luarocks install luafilesystem
```

Dann können Sie das folgende Beispiel verwenden, um die Existenz eines Verzeichnisses zu überprüfen:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr und attr.mode == "directory"
end

-- Überprüfen, ob ein bestimmtes Verzeichnis existiert
if directoryExists("/path/to/your/directory") then
    print("Verzeichnis existiert.")
else
    print("Verzeichnis existiert nicht.")
end
```

Das wird ausgegeben:

```
Verzeichnis existiert.
```

Oder, falls das Verzeichnis nicht existiert:

```
Verzeichnis existiert nicht.
```

Dieser Ansatz nutzt die Funktion `lfs.attributes`, um die Attribute des Pfades zu erhalten. Wenn der Pfad existiert und sein `mode` Attribut `directory` ist, bestätigt es die Existenz des Verzeichnisses.
