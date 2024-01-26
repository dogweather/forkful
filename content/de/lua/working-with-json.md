---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für JavaScript Object Notation. Es ist ein Format zum Speichern und Übertragen von Daten. Programmierer nutzen JSON, weil es menschenlesbar und maschinenverarbeitbar ist, ideal für Datenaustausch zwischen Server und Webanwendungen.

## How to:
Um mit JSON in Lua zu arbeiten, benutzen wir das `dkjson` Modul:

```Lua
local json = require "dkjson"

-- JSON String in Lua table umwandeln
local str = '{"name": "Max", "age": 25, "hobbies": ["Schach", "Radfahren"]}'
local table = json.decode(str)
print(table.name)  -- Ausgabe: Max

-- Lua table in JSON String umwandeln
local tbl = {name = "Max", age = 25, hobbies = {"Schach", "Radfahren"}}
local jsonString = json.encode(tbl)
print(jsonString)  -- Ausgabe: {"name":"Max","age":25,"hobbies":["Schach","Radfahren"]}
```

## Deep Dive
JSON wurde Anfang der 2000er als Teil von JavaScript entworfen, doch seine Einfachheit machte es schnell sprachunabhängig. Lua-Alternativen zu `dkjson` sind `cjson` und `lua-json`. `dkjson` ist in reinem Lua geschrieben, was es portabler, aber langsamer als `cjson` macht, das in C geschrieben ist. Details wie das Behandeln von Groß- und Kleinschreibung bei Schlüsseln oder das Serialisieren von Funktionen hängen von der gewählten Bibliothek ab.

## See Also
- [dkjson Dokumentation](http://dkolf.de/src/dkjson-lua.fsl/home)
- [JSON.org](https://www.json.org/json-de.html) – Informationen über JSON
- [Lua-users Wiki: JSON Modules](http://lua-users.org/wiki/JsonModules) – Übersicht verschiedener Lua JSON-Module
