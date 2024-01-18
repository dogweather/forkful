---
title:                "Zeichenfolgen zusammenfügen"
html_title:           "Lua: Zeichenfolgen zusammenfügen"
simple_title:         "Zeichenfolgen zusammenfügen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was ist das & Warum?
Das Zusammenfügen von Zeichenketten ist ein häufig verwendetes Konzept in der Programmierung. Es bezieht sich auf das Verbinden von zwei oder mehr Zeichenketten zu einer einzigen Zeichenkette. Programmierer nutzen diese Technik, um dynamische Texte zu erstellen, die auf bestimmte Bedingungen oder Eingaben reagieren können.

## So geht's:
```Lua
-- Beispiel 1: Einfache Konkatenation
local str1 = "Hallo"
local str2 = "Welt"
print(str1 .. str2) -- Ausgabe: HalloWelt

-- Beispiel 2: Verwendung von Variablen und Konkatenation
local name = "Marie"
local gruss = "Hallo, " .. name .. "! Willkommen zurück."
print(gruss) -- Ausgabe: Hallo, Marie! Willkommen zurück.
```

## Tiefere Einblicke:
Um eine Zeichenkette in Lua zu verkettungen, kann der Operator ".." verwendet werden. Dieser Operator kann auch verwendet werden, um andere Datentypen wie Zahlen oder Booleans zu verketten, da Lua automatisch eine Zeichenkettenrepräsentation davon erstellt. Eine alternative Methode wäre die Verwendung der string.format-Funktion, um eine formatierte Zeichenkette zu erstellen. Bei der Konkatenation von großen Zeichenketten ist es effizienter, den Konstruktor table.concat zu verwenden.

## Siehe auch:
- [Lua 5.4 Referenzhandbuch](https://www.lua.org/manual/5.4/)
- [Code Academy: Python String Concatenation](https://www.codecademy.com/articles/string-concatenation)
- [Wikipedia: Zeichenkettenverkettung](https://de.wikipedia.org/wiki/Zeichenkettenverkettung)