---
title:                "Zeichenketten verknüpfen"
aliases:
- /de/lua/concatenating-strings/
date:                  2024-01-20T17:35:29.281721-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Konkatenation ist das Zusammenfügen von zwei oder mehreren Zeichenketten (Strings). Programmierer nutzen das, um dynamische Texte zu erzeugen oder Daten sinnvoll zu formatieren.

## How to:
Lua macht's einfach. Nehmen wir an, du hast zwei Strings und willst sie zusammenfügen. Hier ist, wie das geht:

```lua
-- String-Konkatenation mit dem .. Operator
local begruessung = "Hallo"
local name = "Welt"
local vollstaendigeBegruessung = begruessung .. ", " .. name .. "!"

print(vollstaendigeBegruessung)  -- Ausgabe: Hallo, Welt!
```

Wenn du mit Zahlen arbeitest, musst du sie erst in Strings umwandeln:

```lua
local zahl1 = 5
local zahl2 = 10
local ergebnisString = "Ergebnis: " .. tostring(zahl1) .. " + " .. tostring(zahl2) .. " = " .. tostring(zahl1 + zahl2)

print(ergebnisString)  -- Ausgabe: Ergebnis: 5 + 10 = 15
```

## Deep Dive
String-Konkatenation ist nicht neu. Schon seit den ersten Programmiersprachen gibt es diese Möglichkeit. In Lua ist der `..` Operator zuständig für's Zusammenfügen. Lua kümmert sich intern um Speicher und Performance, aber bei großen Datenmengen kann Konkatenation langsam werden. Lua 5.3 führte die `table.concat`-Funktion ein, die effizienter sein kann:

```lua
local teile = {"Lua", "ist", "toll"}
local satz = table.concat(teile, " ")

print(satz)  -- Ausgabe: Lua ist toll
```

Alternativ kannst du auch den `string.format`-Ansatz verwenden, der mehr Kontrolle über das Format bietet:

```lua
local temperatur = 23.4
local nachricht = string.format("Die aktuelle Temperatur beträgt %.1f Grad Celsius.", temperatur)

print(nachricht)  -- Ausgabe: Die aktuelle Temperatur beträgt 23.4 Grad Celsius.
```

## See Also
Interessante Ressourcen für tiefergehende Informationen:

- String Manipulation in Lua: [http://www.lua.org/pil/20.1.html](http://www.lua.org/pil/20.1.html)
