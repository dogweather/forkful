---
title:                "Überprüfung, ob ein Verzeichnis vorhanden ist."
html_title:           "Lua: Überprüfung, ob ein Verzeichnis vorhanden ist."
simple_title:         "Überprüfung, ob ein Verzeichnis vorhanden ist."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion für Programmierer. Es ermöglicht dem Programm, zu prüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor es darauf zugreift. Dies ist besonders hilfreich, um sicherzustellen, dass das Programm nicht abstürzt, wenn das Verzeichnis fehlt.

# Wie geht's?

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Funktion `lfs.attributes()` aus der Lua Bibliothek "LuaFileSystem" verwenden. Diese Funktion gibt ein Table mit Informationen über die angegebene Datei oder das Verzeichnis zurück. Hier ist ein Beispielcode, um zu überprüfen, ob das Verzeichnis "Dokumente" im aktuellen Arbeitsverzeichnis existiert:

```Lua
local lfs = require("lfs")
local docDir = "Dokumente"

local attr = lfs.attributes(docDir) --überprüft, ob "Dokumente" existiert

if attr then --wenn das Verzeichnis existiert
  print("Das Verzeichnis existiert!")
else
  print("Das Verzeichnis existiert nicht!")
end

```

Die Ausgabe wird je nachdem, ob das Verzeichnis existiert oder nicht, entweder "Das Verzeichnis existiert!" oder "Das Verzeichnis existiert nicht!" sein.

# Tiefer eintauchen

Die Funktion `lfs.attributes()` wurde in LuaFileSystem Bibliothek eingeführt, um Dateiattribute zu erhalten, wie z.B. Größe, Änderungsdatum und Zugriffsrechte. Das Überprüfen, ob ein Verzeichnis existiert, ist eine typische Anwendung dieser Funktion. Eine Alternative zum Überprüfen der Existenz eines Verzeichnisses ist die Verwendung der Funktion `lfs.chdir()`, um in das Verzeichnis zu wechseln und dann zu überprüfen, ob es erfolgreich war.

Bei der Verwendung von `lfs.attributes()` muss beachtet werden, dass die Funktion möglicherweise nicht auf allen Betriebssystemen gleich funktioniert und die zurückgegebenen Attribute variieren können.

# Auch interessant

Weitere Informationen und Beispiele zum Umgang mit Dateien und Verzeichnissen in Lua finden Sie in der offiziellen Dokumentation der Sprache: https://www.lua.org/manual/5.3/manual.html#6.8

Für eine vollständige Referenz zur LuaFileSystem Bibliothek, besuchen Sie die offizielle Website: https://keplerproject.github.io/luafilesystem/