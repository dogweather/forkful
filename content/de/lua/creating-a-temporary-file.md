---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Temporäre Dateien in Lua: Eine einfache Anleitung

## Was & Warum?

Ein temporäre Datei wird vom Programm erstellt und zeitweilig genutzt, um temporäre Daten aufzunehmen - etwa bei komplexen Berechnungen oder Datenverarbeitungsaufgaben. Sie entlasten den Arbeitsspeicher und machen Abläufe effizienter.

## Wie man's macht:

In Lua, kannst du eine temporäre Datei ganz leicht erzeugen und benutzen. Siehe hier:

```Lua
-- Eine temporäre Datei erzeugen
local tmp = os.tmpname()

-- In der temporären Datei schreiben
local file = io.open(tmp, "w")
file:write("Hallo Welt!")
file:close()

-- Aus der temporären Datei lesen
local file = io.open(tmp, "r")
local data = file:read("*a")
file:close()

print(data)
```

Wenn du das obige Skript ausführst, siehst du:

```Lua
Hallo Welt!
```

## Deep Dive

Historisch gesehen hat Lua bereits in früheren Versionen die Möglichkeit geboten, temporäre Dateien zu erzeugen, was seine Flexibilität und Benutzerfreundlichkeit gegenüber anderen Skriptsprachen unterstreicht.  

Alternativen, um temporäre Dateien zu erzeugen, sind z.B. die Lua-Module "LuaFileSystem" oder "lua-posix". Diese bieten weitere Funktionen und Möglichkeiten. Allerdings, für einfache Anforderungen ist `os.tmpname()` völlig ausreichend.

In Bezug auf die Implementierungsdetails, `os.tmpname()` gibt einfach nur einen einzigartigen Dateinamen zurück, die dann mit `io.open` geöffnet werden kann. Wo die temporäre Datei physisch gespeichert wird, hängt vom Betriebssystem und den Systemeinstellungen ab.

## Siehe auch 

- [Lua 5.2 Referenzhandbuch - os.tmpname()](https://www.lua.org/manual/5.2/manual.html#pdf-os.tmpname)
- [Lua-Users Wiki: Temporary Files](http://lua-users.org/wiki/TemporaryFiles)