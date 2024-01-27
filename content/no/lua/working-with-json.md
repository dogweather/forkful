---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON (JavaScript Object Notation) er et datadelingsformat brukt for å lagre og utveksle data. Programmerere bruker JSON fordi det er lettleselig for mennesker og lett å tolke for maskiner.

## Hvordan gjøre:
For å jobbe med JSON i Lua, trenger du et bibliotek som `dkjson` eller `cjson`. Her er hvordan du håndterer JSON:

```Lua
-- Inkluderer et JSON-bibliotek
local json = require "dkjson"

-- Konverterer et Lua-tabell til JSON
local tabell = { navn = "Ola", alder = 28, programmerer = true }
local json_str = json.encode(tabell)
print(json_str) -- Output: {"navn":"Ola","alder":28,"programmerer":true}

-- Tolker en JSON-streng til et Lua-tabell
local json_data = '{"navn":"Kari","alder":25,"programmerer":false}'
local tabell_data = json.decode(json_data)
print(tabell_data.navn) -- Output: Kari
```

## Dypdykk
JSON ble opprettet av Douglas Crockford på tidlig 2000-tallet. Alternativer til JSON inkluderer XML og YAML, men JSON er populært fordi det passer godt med web applikasjoner. Det er også generelt raskere og mer kompakt. I Lua, implementeres JSON tolking og generering vanligvis gjennom eksterne biblioteker fordi standardbiblioteket ikke inneholder støtte for JSON.

## Se Også
- `dkjson` bibliotek: http://dkolf.de/src/dkjson-lua.fsl/home
- `cjson` bibliotek: https://www.kyne.com.au/~mark/software/lua-cjson.php
- Offisiell JSON-nettside: https://www.json.org/json-no.html
- Lua brukerveiledning: https://www.lua.org/manual/5.4/

Husk å sjekke dokumentasjonen for det biblioteket du velger for avanserte funksjoner og ytterligere brukseksempler.
