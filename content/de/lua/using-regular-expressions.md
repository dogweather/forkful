---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, auch Regex genannt, sind Muster, mit denen man Text nach bestimmten Regeln durchsucht, ersetzt oder aufteilt. Programmierer nutzen sie, weil sie mächtige Werkzeuge sind, um komplexe Textbearbeitungen effizient durchzuführen.

## How to:
Lua verwendet Patterns, die ähnlich, aber nicht so umfangreich wie reguläre Ausdrücke sind. Hier ein paar Beispiele:

Suchen:
```Lua
local text = "Hallo Welt!"
if string.match(text, "Welt") then
  print("Gefunden!")
end
-- Ausgabe: Gefunden!
```

Ersetzen:
```Lua
local text = "Hallo Welt!"
local neuer_text = string.gsub(text, "Welt", "Lua")
print(neuer_text)
-- Ausgabe: Hallo Lua!
```

Zerlegen:
```Lua
local text = "Anna:Berta:Charlie"
local namen = {}
for name in string.gmatch(text, "[^:]+") do
  table.insert(namen, name)
end
print(table.concat(namen, ", "))
-- Ausgabe: Anna, Berta, Charlie
```

## Deep Dive
Reguläre Ausdrücke, wie in vielen anderen Sprachen bekannt, wären in Lua eine Überfrachtung gewesen, also entschieden sich die Entwickler für einfachere Muster (patterns). Alternativen zu Luas eingebauten Patterns sind externe Bibliotheken wie `LuaPCRE` oder `rex`. Diese bieten vollständige Regex-Unterstützung, erfordern aber zusätzliche Installationen. Bei der Performance hängt es vom Anwendungsfall ab – eingebaute Patterns sind in Lua oft schneller, während komplexere Regex-Operationen mithilfe externer Bibliotheken oftmals besser gehandhabt werden können.

## See Also
- Lua 5.4 Referenzhandbuch: https://www.lua.org/manual/5.4/
- Lua Users Wiki zu Patterns: http://lua-users.org/wiki/PatternsTutorial
- LuaPCRE auf GitHub: https://github.com/rrthomas/luapcre
- rexlib: http://rrthomas.github.io/lrexlib/
