---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings ist der Prozess des Auslesens bestimmter Teile eines Strings. Programmierer machen das alltäglich, um relevante Daten aus umfangreichen Zeichenketten zu isolieren und zu manipulieren.

## So geht's:
Die Funktion `string.sub` in Lua wird verwendet, um Teilstrings zu extrahieren. Die Syntax der Funktion ist `string.sub(string, start, end)`. Hier sind einige Beispiele.

```Lua
s = "Hallo Welt"
print(string.sub(s, 1, 5))  --> Gibt "Hallo" aus
print(string.sub(s, 7))  --> Gibt "Welt" aus
```

## Deep Dive:
Das Extrahieren von Teilstrings ist seit Langem ein grundlegender Bestandteil der Programmierung. In Lua, einer Sprache, die 1993 eingeführt wurde, wird diese Funktion durch die native Bibliothek 'string' unterstützt.

Es gibt auch andere alternative Funktionen für das Extrahieren von Teilstrings in Lua wie `string.find` und `string.match`.

Die Implementierung der Funktion `string.sub` ist effizient und einfach zu verwenden, aber sie beginnt auch bei 1, nicht bei 0, was einzigartig für Lua ist.

## Siehe Auch:
Weitere Informationen und Beispiele für Lua finden Sie in der offiziellen Lua-Dokumentation unter https://www.lua.org/manual/5.4/manual.html.
Außerdem ist das Buch "Programming in Lua" (https://www.lua.org/pil/contents.html) eine nützliche Ressource, da es verschiedene Aspekte von Lua, einschließlich der String-Behandlung, gründlich abdeckt.