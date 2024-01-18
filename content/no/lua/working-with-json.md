---
title:                "Arbeide med json"
html_title:           "Lua: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-json.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Arbeidet med JSON handler om å håndtere data i et spesifikt format som kalles JSON (JavaScript Object Notation). JSON er et populært format blant programmerere på grunn av sin enkelhet og lesbarhet. Ved å konvertere data til JSON-format, kan programmerere enkelt lagre, organisere og utveksle informasjon mellom forskjellige programmer og systemer.

# Hvordan:
Kodingseksempler og eksempelutgang innen ```Lua...``` kodeblokker:

1. Kode for å konvertere en Lua-table til JSON-format:

```Lua
local json = require("json")  -- importerer JSON-biblioteket
local data = {              -- opprett en Lua-table
    navn = "Per",
    alder = 30,
    yrke = "programmerer"
}
local jsonData = json.encode(data)   -- konverterer tabellen til JSON-format
print(jsonData)   -- skriver ut JSON-dataene til konsollen
```
Eksempelutgang: ```{"navn":"Per","alder":30,"yrke":"programmerer"}```

2. Kode for å parse en JSON-streng til en Lua-table:

```Lua
local json = require("json")  -- importerer JSON-biblioteket
local jsonData = [[{"farge":"blå","type":"bil","merke":"Tesla"}]]  -- en JSON-streng
local data = json.decode(jsonData)  -- konverterer strengen til en Lua-table
print(data.type)   -- skriver ut verdien som er lagret under "type" i tabellen
```
Eksempelutgang: ```bil```

# Dypdykk:
1. Historisk kontekst: JSON ble utviklet på begynnelsen av 2000-tallet som en enkel måte å representere data på weben. Det ble raskt populært og er nå et standardisert format for å utveksle data mellom programmer og systemer.

2. Alternativer: JSON er ikke det eneste formatet som brukes til å representere data. Andre populære formater inkluderer CSV, XML og YAML. Hver har sine egne fordeler og ulemper, men det som gjør JSON unikt er dens enkelhet og lette lesbarhet.

3. Implementeringsdetaljer: Lua tilbyr et innebygd JSON-bibliotek som gjør det enkelt å konvertere data til og fra JSON-format. Dette biblioteket er også kompatibelt med andre Lua-rammeverk og biblioteker.

# Se også:
- [Lua JSON dokumentasjon](http://luaforge.net/projects/luajson/)
- [Offisiell JSON-nettside](https://www.json.org/)
- [Hva er JSON og hvorfor er det så populært?](https://medium.com/swlh/what-is-json-and-why-is-it-so-popular-9aa7d37ce06a) (engelsk)