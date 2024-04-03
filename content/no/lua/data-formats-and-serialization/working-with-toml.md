---
date: 2024-01-26 04:24:18.978698-07:00
description: "Hvordan: F\xF8rst, s\xF8rg for at Lua-milj\xF8et ditt har en TOML-parser.\
  \ Vi vil bruke `lua-toml` for dette eksemplet."
lastmod: '2024-03-13T22:44:40.953910-06:00'
model: gpt-4-0125-preview
summary: "F\xF8rst, s\xF8rg for at Lua-milj\xF8et ditt har en TOML-parser."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
Først, sørg for at Lua-miljøet ditt har en TOML-parser. Vi vil bruke `lua-toml` for dette eksemplet.

```Lua
local toml = require("toml")

-- Parse TOML-streng
local toml_data = [[
title = "TOML Eksempel"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Eksempel"

-- Generer TOML-streng
local table_data = {
  title = "TOML Eksempel",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Eksempel på utskrift:
```
TOML Eksempel
```

## Dypdykk
TOML ble skapt av Tom Preston-Werner i 2013 som et alternativ til andre dataserialiseringsspråk som XML og YAML, og tilbyr et mer rettfram format for å representere konfigurasjonsdata. Selv om JSON er allestedsnærværende, kan dets syntaks være omstendelig for konfigurasjonsfiler. TOML utmerker seg med en klarere syntaks for mennesker, som ligner på .ini-filer, men med muligheter for nesting og datatyper.

Alternativer til TOML inkluderer JSON, YAML og XML. Men TOML er spesielt designet for konfig og er argumenterbart enklere enn YAML, mer lesbar enn JSON til konfigformål, og mindre omstendelig enn XML.

Å implementere TOML-håndtering i Lua krever vanligvis et tredjeparts bibliotek. Ytelse og funksjoner kan variere, fra grunnleggende parsing til full støtte for serialisering. Når man håndterer store konfigurasjonsfiler eller hyppige lese-/skriveoperasjoner, bør man vurdere bibliotekets ytelse og overensstemmelse med den nyeste TOML-versjonen.

## Se Også
- TOML Spesifikasjon: https://toml.io/en/
- `lua-toml` bibliotek: https://github.com/jonstoler/lua-toml
- Sammenligning av data-serialiseringsformater: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
