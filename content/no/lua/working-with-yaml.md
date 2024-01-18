---
title:                "Å jobbe med YAML"
html_title:           "Lua: Å jobbe med YAML"
simple_title:         "Å jobbe med YAML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

Hva & hvorfor? 
YAML står for "YAML Ain't Markup Language," og det er et enkelt og leselig format for å representere datar strukturert som nøkkel-verdi par. Dette gjør YAML til et populært verktøy blant programmere fordi det er enkelt å forstå og bruke.

Hvordan:
Lua har innebygde funksjoner for å enkelt lese og skrive YAML-filer. For å lese en YAML-fil i Lua, bruk følgende kode:
```Lua
local yaml = require("yaml")
local file = io.open("eksempel.yaml", "r") -- Åpner en fil med navn "eksempel.yaml" for lesing
local data = yaml.load(file:read("*all")) -- Leser og konverterer YAML-dataen til en Lua-tabell
file:close() -- Lukker filen når vi er ferdige
```
For å skrive en YAML-fil i Lua, bruk denne koden som et eksempel:
```Lua
local yaml = require("yaml")
local data = {navn = "John Doe", alder = 30} -- Lager en Lua-tabell med nøkkel-verdi par
local file = io.open("eksempel.yaml", "w") -- Åpner en fil med navn "eksempel.yaml" for skriving
file:write(yaml.dump(data)) -- Konverterer og skriver dataen til filen
file:close() -- Lukker filen når vi er ferdige
```

Dykk ned:
YAML ble opprinnelig utviklet for å være en enklere og mer leselig formattering alternativ til andre strukturerte dataformater som JSON og XML. Det brukes ofte til å konfigurere programmer og konvertere data mellom forskjellige filer og systemer. Alternativene til YAML inkluderer JSON, XML og andre programmeringsspråk spesifikke formater.

Se også:
LUA YAML-bibliotek: https://github.com/golgote/lua-yaml
Offisiell YAML-dokumentasjon: https://yaml.org/spec/1.2/spec.html