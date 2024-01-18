---
title:                "Att arbeta med yaml"
html_title:           "Lua: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att arbeta med YAML är ett sätt för programmerare att strukturera och lagra data i ett läsbart och lättanvänt format. Detta gör att det blir lättare att hantera och kommunicera mellan olika delar av en applikation eller system.

## Hur gör man:
### Exempel 1:
```lua
local file = io.open("data.yaml", "r") -- öppnar YAML-filen för läsning
local content = file:read("*all") -- läser in allt innehåll från filen
file:close() -- stänger filen

-- konverterar YAML till en Lua-tabell
local yaml = require("yaml") -- importerar YAML-biblioteket
local data = yaml.load(content) -- konverterar innehållet från YAML till en tabell
print(data.name) -- skriver ut namnet som finns i tabellen
```
#### Yaml-fil
```yaml
name: John Doe
age: 35
occupation: programmer
```

#### Output
```
John Doe
```

### Exempel 2:
```lua
local data = {
    name = "Anna Smith",
    age = 27,
    occupation = "graphic designer"
}

-- konverterar Lua-tabell till YAML
local yaml = require("yaml") -- importerar YAML-biblioteket
local content = yaml.dump(data) -- konverterar tabellen till YAML-sträng
print(content) -- skriver ut YAML-strängen
```

#### Output
```
name: Anna Smith
age: 27
occupation: graphic designer
```

## Djupdykning:
### Historisk kontext:
YAML är ett akronym för "YAML Ain't Markup Language" och har funnits sedan 2001. Det är ett textbaserat format som är lättläst för både människor och maskiner. Det används ofta för applikationskonfiguration eller att skicka och ta emot data mellan system.

### Alternativ:
YAML är inte det enda alternativet för strukturerad data. Andra populära format inkluderar JSON och XML. Både YAML och JSON är mer lättlästa och föredragna av programmerare jämfört med XML.

### Implementation:
Det finns olika implementationer av YAML för olika programmeringsspråk, inklusive Lua. Det finns även många bibliotek och verktyg som kan hjälpa till med att läsa, skriva och manipulera YAML-filer.

## Se även:
- [YAML.org](https://yaml.org/) - officiell hemsida för YAML
- [LuaYAML](https://github.com/davidm/lua-yaml) - YAML-bibliotek för Lua
- [YAML lint](http://www.yamllint.com/) - verktyg för att validera YAML-filer