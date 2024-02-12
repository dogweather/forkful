---
title:                "Suchen und Ersetzen von Text"
aliases:
- /de/lua/searching-and-replacing-text.md
date:                  2024-01-20T17:58:09.097304-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen sind Kernfunktionen in der Textbearbeitung: Man findet spezifische Textmuster und ersetzt sie durch andere Inhalte. Programmierer nutzen dies, um Code zu korrigieren, Daten zu bereinigen oder automatische Anpassungen vorzunehmen.

## So geht's:
Hier ist ein einfacher Lua-Code, der zeigt, wie man Text sucht und ersetzt. Die Funktion `string.gsub` ist dein Freund.

```lua
local text = "Hallo Welt! Hallo Lua! Hallo Programmierer!"
local suchmuster = "Hallo"
local ersatz = "Tschüss"

local neuer_text, anzahl_der_ersetzungen = string.gsub(text, suchmuster, ersatz)

print(neuer_text)  -- Tschüss Welt! Tschüss Lua! Tschüss Programmierer!
print("Anzahl der Ersetzungen:", anzahl_der_ersetzungen)  -- 3
```

## Tiefgang:
Das Suchen und Ersetzen von Text existiert seit den frühen Tagen der Informatik. Lua's `string.gsub` Funktion basiert auf Musterabgleich, ähnlich regulären Ausdrücken, hat aber eine einfachere Syntax. Alternativ könnte man `string.find` für das Suchen und dann Lua-Schleifen und Stringmanipulationsfunktionen für das Ersetzen verwenden, aber das ist umständlicher. Bei `string.gsub` sollte beachtet werden, dass die Funktion zuerst den kompletten String durchläuft und danach Ersetzungen vornimmt, was bei großen Textmengen ein Performancethema sein kann.

## Siehe Auch:
- Lua 5.4 Referenzhandbuch: https://www.lua.org/manual/5.4/
- Online Lua Demos (zum Ausprobieren von Code): https://www.lua.org/demo.html
- Einführung in Lua Muster: https://www.lua.org/pil/20.2.html
