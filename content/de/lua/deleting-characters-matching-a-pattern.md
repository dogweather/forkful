---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ist das Entfernen bestimmter Zeichen aus einer Zeichenkette. Programmierer machen das, um unerwünschte Daten zu entfernen und Datensätze zu bereinigen.

## So Geht's:
Schauen wir uns jetzt an, wie man in Lua Zeichen löscht, die einem Muster entsprechen:

```Lua
--Zeichenkette definieren
local str = "Apfel, Banana, Kirsche, Dattel, Erdbeere"
-- unerwünschte Zeichen entfernen
local neuerStr = str:gsub(",", "")
print(neuerStr)
```
Die Ausgabe wäre:

```Lua
"Apfel Banana Kirsche Dattel Erdbeere"
```
Der Code verwendet die Lua `gsub`-Funktion, um alle Kommas in der Zeichenkette zu entfernen.

## Vertiefung
Historisch gesehen stammt das Musterlöschkonzept aus den Unix-basierten Systemen mit Programmen wie 'sed' und 'grep'. Alternativ können wir auch die `string.gsub` Funktion in Lua verwenden, um bestimmte Muster zu bearbeiten oder zu ersetzen. Während der Ausführung prüft `gsub`, ob das Muster in der Zeichenkette vorhanden ist, löscht es und gibt die bearbeitete Zeichenkette zurück.

## Siehe Auch
Es gibt viel mehr zu entdecken, wenn es um Muster und Zeichenkettenmanipulation in Lua geht.

1. Die komplette Dokumentation finden Sie [hier](http://www.lua.org/manual/5.4/manual.html#6.4.1)

2. Weitere Tutorials und Beispiele sind [hier](https://www.tutorialspoint.com/lua/lua_strings.htm) verfügbar.

3. Einige spezifische Zeichenkettenfunktionen, die in Lua verwendet werden, können Sie [hier](https://www.lua.org/pil/20.html) nachsehen.