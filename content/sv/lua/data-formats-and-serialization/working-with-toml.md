---
date: 2024-01-26 04:24:19.405818-07:00
description: "Hur man g\xF6r: F\xF6rst, se till att din Lua-milj\xF6 har en TOML-tolkare.\
  \ Vi anv\xE4nder `lua-toml` f\xF6r det h\xE4r exemplet."
lastmod: '2024-03-13T22:44:38.062197-06:00'
model: gpt-4-0125-preview
summary: "F\xF6rst, se till att din Lua-milj\xF6 har en TOML-tolkare."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
Först, se till att din Lua-miljö har en TOML-tolkare. Vi använder `lua-toml` för det här exemplet.

```Lua
local toml = require("toml")

-- Tolka TOML-sträng
local toml_data = [[
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Example"

-- Generera TOML-sträng
local table_data = {
  title = "TOML Example",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Exempelutdata:
```
TOML Example
```

## Djupdykning
TOML skapades av Tom Preston-Werner år 2013 som ett alternativ till andra data-serialiseringsspråk som XML och YAML, och erbjuder ett enklare format för att representera konfigurationsdata. Medan JSON är allestädes närvarande kan dess syntax vara bökig för konfigurationsfiler. TOML utmärker sig med en tydligare syntax för människor, som liknar .ini-filer men med möjligheter till nesting och datatyper.

Alternativ till TOML inkluderar JSON, YAML och XML. Dock är TOML specifikt utformat för konfig och är argumenterbart enklare än YAML, mer läsbart än JSON för konfigurationsändamål, och mindre långdraget än XML.

Att implementera TOML-hantering i Lua kräver vanligtvis ett tredjepartbibliotek. Prestanda och funktioner kan variera, från grundläggande tolkning till fullständigt serialiseringsstöd. När du arbetar med stora konfigurationsfiler eller frekventa läs/skriv-operationer, överväg bibliotekets prestanda och överensstämmelse med den senaste versionen av TOML.

## Se även
- TOML-specifikation: https://toml.io/en/
- `lua-toml` bibliotek: https://github.com/jonstoler/lua-toml
- Jämförelse av Data-serialiseringsformat: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
