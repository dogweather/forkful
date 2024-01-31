---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:57:43.410152-07:00
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, bedeutet zu kontrollieren, ob ein bestimmter Ordnerpfad auf dem Dateisystem vorhanden ist. Das ist wichtig, um Fehler zu vermeiden, wenn man Dateien speichert oder liest und das genannte Verzeichnis benötigt wird.

## So geht’s:
Lua bietet keine eingebaute Funktion für das Überprüfen von Verzeichnissen, aber wir können das `lfs` (LuaFileSystem) Modul verwenden oder auf OS-spezifische Befehle zurückgreifen.

Beispiel mit LuaFileSystem (lfs):
```Lua
local lfs = require('lfs')

function directory_exists(path)
    local ok, err, code = os.rename(path, path)
    if not ok then
        if code == 13 then
            -- Pfad existiert, ist aber ein Verzeichnis
            return true
        end
        return false
    end
    -- Überprüfe, ob der Pfad wirklich ein Verzeichnis ist
    return lfs.attributes(path, "mode") == "directory"
end

-- Beispielhafter Aufruf
if directory_exists("/pfad/zum/verzeichnis") then
    print("Verzeichnis existiert.")
else
    print("Verzeichnis existiert nicht.")
end
```
Ausgabe könnte sein:
```
Verzeichnis existiert.
```
oder
```
Verzeichnis existiert nicht.
```

## Deep Dive
In früheren Lua-Versionen mussten Entwickler oft auf umständlichere Methoden zurückgreifen, wie z.B. os.execute mit system-spezifischen Befehlen. Das `lfs`-Modul vereinfacht diese Aufgabe, bietet aber eine zusätzliche Abhängigkeit.

Alternativen ohne `lfs`:
- Plattrformspezifische Skripting, z.B. `io.popen("if exist mydir echo 1")` für Windows
- `os.execute` und ähnliche Funktionen für Shell-Befehle

Beim Implementieren einer Funktion, die das Vorhandensein eines Verzeichnisses überprüft, ist es wichtig, auf Berechtigungsfehler (Code 13) zu achten, da diese darauf hinweisen können, dass das Verzeichnis existiert, aber nicht modifiziert werden kann.

## Siehe Auch:
- LuaFileSystem Dokumentation: https://keplerproject.github.io/luafilesystem/
- Lua 5.4 Referenzmanual: https://www.lua.org/manual/5.4/
- Stack Overflow: Diskussionen und Lösungen zu "check if a directory exists in Lua"
