---
title:                "Arbeiten mit yaml"
html_title:           "Lua: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein Dateiformat, das für die Lesbarkeit von Maschinen und Menschen gleichermaßen entwickelt wurde. Es wird häufig von Programmierern verwendet, um Konfigurationsdateien zu speichern oder Daten zwischen verschiedenen Programmen auszutauschen.

## How to:
Das Parsen von YAML-Dateien in Lua ist einfach und unkompliziert. Hier ist ein Beispiel, wie man eine YAML-Datei öffnet und ihre Inhalte in eine Lua-Tabelle lädt:
```Lua
local yaml = require("yaml") -- Modul zum Parsen von YAML importieren
local file = io.open("config.yaml", "r") -- YAML-Datei öffnen
local data = yaml.parse(file:read("*all")) -- Inhalt der YAML-Datei parsen und in Lua-Tabelle laden
file:close() -- Datei schließen
```
Die Variable "data" enthält nun alle Daten aus der YAML-Datei. Zum Beispiel, wenn die Datei folgende Inhalte hat:
```YAML
name: John
age: 25
hobbies:
- coding
- reading
```
Werden sie in Lua wie folgt gespeichert:
```Lua
data = {
  name = "John",
  age = 25,
  hobbies = {"coding", "reading"}
}
```

## Deep Dive:
YAML steht für "YAML Ain't Markup Language" und wurde ursprünglich als Alternativformat zu XML entwickelt. Im Vergleich zu XML ist YAML jedoch leichter lesbar und weniger verbose, was es für Programmierer attraktiver macht.

Alternativ zu YAML können Programmierer auch JSON verwenden, um Daten zu speichern und austauschen. Beide Formate haben ihre Vor- und Nachteile, aber YAML ist aufgrund seiner Einfachheit und Lesbarkeit bei vielen Programmierern beliebter.

Das oben gezeigte Beispiel verwendet das Lua-Modul "yaml" zum Parsen von YAML-Dateien. Dieses Modul ist Teil des "LuaRocks"-Projekts, das eine große Sammlung von Lua-Modulen zur Verfügung stellt. Es kann über den Befehl "luarocks install yaml" installiert werden.

## Siehe auch:
- [YAML-Homepage](https://yaml.org/)
- [LuaRocks-Projekt](https://luarocks.org/)
- [Offizielle Lua-Dokumentation](https://www.lua.org/docs.html)