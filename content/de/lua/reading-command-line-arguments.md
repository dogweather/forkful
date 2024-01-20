---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was und Warum?

Beim Lesen von Befehlszeilenargumenten nimmt Ihr Lua-Skript Daten vom Benutzer direkt aus der Befehlszeile in der Konsole auf. Das ermöglicht eine effizientere Interaktion mit dem Benutzer und eine automatisierte Skriptausführung.

## So funktioniert's:

Befehlszeilenargumente werden in Lua in die globale Tabelle `arg` gestellt. Hier ist ein einfaches Beispiel:

```Lua
-- Drucken aller Argumente
for i in ipairs(arg) do 
    print(arg[i]) 
end
```

Führen Sie dieses Skript mit: `lua script.lua arg1 arg2 arg3`. Der Output wird so aussehen:

```Lua
script.lua
arg1
arg2
arg3
```

Die Tabelle `arg` hat auch negative Indizes. `arg[-1]` beinhaltet z.B. den Interpreter-Namen.

## Tieferer Einblick

Die Einbindung von Befehlszeilenargumenten in Lua ist relativ neu. In älteren Versionen (vor Version 5.1) mussten Sie die Bibliothek `lfs` verwenden. Alternativen zur direkten Einbindung sind das Einlesen von Benutzereingaben während der Skriptausführung oder über eine GUI. Beachten Sie jedoch, dass Befehlszeilenargumente in Lua Zeichenkettentypen sind. Sie müssen sie selbst umformen, falls Sie andere Datentypen benötigen.

## Siehe auch

1. Offizielle Lua-Dokumentation: https://www.lua.org/manual/5.4/manual.html
2. Für mehr Detail zu `arg` Tabelle: https://www.lua.org/pil/16.html
3. Eine umfangreiche Einführung in Lua: https://learnxinyminutes.com/docs/de-de/lua-de/
5. Für Einsteiger in Lua: https://www.lua.org/start.html