---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein menschenlesbares Datenformat für Konfigurationsdateien und Datenaustausch. Programmierer nutzen es wegen seiner Klarheit und Einfachheit im Vergleich zu JSON oder XML.

## How to:
Zuerst musst du das `lyaml` Modul installieren. Benutze `luarocks`:

```bash
luarocks install lyaml
```

Hier ein einfaches Beispiel, YAML zu laden und zu parsen:

```Lua
local lyaml = require('lyaml')
local yaml_data = [[
- Just
- A simple
- YAML
- List
]]

local lua_table = lyaml.load(yaml_data)
for i, item in ipairs(lua_table) do
  print(i, item)
end
```

Ausgabe:

```plaintext
1 Just
2 A simple
3 YAML
4 List
```

## Deep Dive
YAML, kurz für "YAML Ain't Markup Language", wurde früh in den 2000ern entwickelt. Es gilt als einfacher in der Handhabung als XML und ist oft besser lesbar als JSON. Libraries wie `lyaml` erlauben die Implementierung in Lua. Alternativen zu YAML könnten JSON oder TOML sein, je nach Anwendungszweck.

## See Also
- YAML Spezifikation: https://yaml.org/spec/
- `lyaml` Dokumentation: https://github.com/gvvaughan/lyaml
- LuaRocks Repository: https://luarocks.org/modules/gvvaughan/lyaml