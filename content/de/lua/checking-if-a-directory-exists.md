---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Lua: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Checken, ob ein Verzeichnis existiert in Lua

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein Vorgang, bei dem ein Programm feststellt, ob ein bestimmtes Verzeichnis auf dem System vorhanden ist. Programmierer machen das, um Fehler zu vermeiden, die auftreten können, wenn sie versuchen, auf ein nicht vorhandenes Verzeichnis zuzugreifen.

## So geht's:

In Lua kann man mit `lfs.attributes()` überprüfen, ob ein Verzeichnis existiert. Hier ist ein kurzer Codeausschnitt:

```Lua
local lfs = require('lfs')

function isDirectory(path)
    -- `lfs.attributes(path).mode` returns 'directory' if path is a directory
    return lfs.attributes(path, 'mode') == 'directory'
end

print(isDirectory('/path/to/directory'))  -- false if the directory does not exist
```

## Tiefgehende Betrachtung

Historisch gesehen war es nicht immer einfach, in Lua zu überprüfen, ob ein Verzeichnis existiert. Vorerst war eine direkte Implementierung nicht vorhanden. Man benutzte oft FFI-Bibliotheken oder rief systemeigene Funktionen auf. Mit der Einführung von 'LuaFileSystem' (lfs), einer portablen Bibliothek zur Manipulation von Dateisystemen, wurde es viel einfacher.

Eine Alternative zum obigen Ansatz ist die Verwendung der Funktion `os.execute()`. Aber beachten Sie, dass dieser Befehl systemabhängig ist und möglicherweise nicht auf allen Plattformen funktioniert. 

Die Funktionsweise von `lfs.attributes(path, 'mode')` ist recht einfach. Wenn das Verzeichnis existiert, gibt es 'directory' zurück, andernfalls gibt es nil zurück.

## Siehe auch

Für mehr Informationen über `lfs.attributes()`, siehe die offizielle Laravel-Dokumentation: [https://keplerproject.github.io/luafilesystem/manual.html#attributes](https://keplerproject.github.io/luafilesystem/manual.html#attributes)