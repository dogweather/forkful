---
date: 2024-01-20 17:51:21.238720-07:00
description: "Anleitung: In Lua gibt es keine eingebaute Interpolationsfunktion, daher\
  \ nutzen wir oft die `string.format`-Methode oder die String-Konkatenation. Hier\u2026"
lastmod: '2024-03-13T22:44:54.001303-06:00'
model: gpt-4-1106-preview
summary: In Lua gibt es keine eingebaute Interpolationsfunktion, daher nutzen wir
  oft die `string.format`-Methode oder die String-Konkatenation.
title: Zeichenketten interpolieren
weight: 8
---

## Anleitung:
In Lua gibt es keine eingebaute Interpolationsfunktion, daher nutzen wir oft die `string.format`-Methode oder die String-Konkatenation. Hier sind beide Methoden dargestellt:

```lua
-- String-Konkatenation
local name = "Welt"
local greeting = "Hallo, " .. name .. "!"
print(greeting)  -- Ausgabe: Hallo, Welt!

-- string.format
local name = "Welt"
local greeting = string.format("Hallo, %s!", name)
print(greeting)  -- Ausgabe: Hallo, Welt!
```

## Tief eintauchen:
In älteren oder simplen Skriptsprachen war die direkte Interpolation von Strings gängig, wie in Perl oder Ruby mit `"Hallo, #{name}!"`. In Lua muss man expliziter sein, da die Sprache diese Funktionalität nicht direkt bietet.

Alternativen zur `string.format` sind das Konkatenationsoperator `..` oder, für komplexere Szenarien, eigene Interpolationsfunktionen zu schreiben, die Muster ersetzen.

Die `string.format` Methode ist mächtig, da sie eine Vielzahl an Formatierungsoptionen bietet (ähnlich wie in C's `printf`), was besonders bei Zahlenformatierungen von Vorteil ist.

## Siehe auch:
- Lua-Handbuch zur `string`-Bibliothek: [https://www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- Eine Diskussion über String-Interpolation in Lua auf Stack Overflow: [https://stackoverflow.com/questions/3857601/how-do-i-do-string-interpolation-in-lua](https://stackoverflow.com/questions/3857601/how-do-i-do-string-interpolation-in-lua)
