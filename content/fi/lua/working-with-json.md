---
title:                "Työskentely jsonin kanssa"
html_title:           "Lua: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Työskentely JSONin kanssa tarkoittaa tiedon tallentamista ja jakamista tietorakenteena, joka käyttää valmiiksi määriteltyä syntaksia. JSON on lyhenne sanoista "JavaScript Object Notation" ja sitä käytetään usein siirrettävien tietomuotojen välittämiseen verkossa. Ohjelmoijat käyttävät JSONia helpottaakseen tiedon käsittelyä ja siirtoa eri ohjelmistojen välillä, kuten web-sovelluksissa tai tietokannoissa.

## Kuinka?
Lua-ohjelmoijat voivat käsitellä JSONia käyttämällä erityisiä kirjastoja, kuten "dkjson". Kirjaston avulla voidaan luoda ja lukea JSON-muotoista dataa Lua-koodissa. Esimerkiksi:

```Lua
local json = require "dkjson" -- Ladataan dkjson-kirjasto

-- Luo taulukko JSON-muodossa
local taulukko = {"omena", "banaani", "appelsiini"}

-- Muutetaan taulukko JSON-muotoon
local json_taulukko = json.encode(taulukko)

print(json_taulukko)
```

Tulostaa: ```["omena", "banaani", "appelsiini"]```

## Syvemmälle
JSON syntyi alunperin JavaScript-kielestä, mutta siitä on tullut suosittu tiedon tallennusmuoto eri ohjelmointikielillä, mukaan lukien Lua. Alternatiiveja JSONille ovat esimerkiksi XML ja YAML, mutta JSON on yleensä helpommin luettavissa ja käsiteltävissä. JSON-tiedon lukeminen ja kirjoittaminen Lua-koodissa voi joskus olla haastavaa, mutta kirjastojen käyttö helpottaa tätä prosessia.

## Katso myös
- dkjson-kirjaston dokumentaatio: https://dkolf.de/src/dkjson-lua.fsl/home
- JSON-syntaksia ja käyttöä koskeva opetusohjelma: https://www.json.org/json-fi.html