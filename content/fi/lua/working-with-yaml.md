---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä ja Miksi?"

YAML on datan serialisointikieli, joka on ihmisen luettavissa ja helposti ymmärrettävä. Ohjelmoijat käyttävät sitä konfiguraatiotiedostoihin ja datan vaihtoon, koska se on selkeä ja laajasti yhteensopiva eri teknologioiden kanssa.

## How to:
"Kuinka tehdä:"

```Lua
-- Install yaml module using luarocks or your package manager of choice
-- luarocks install lyaml

local yaml = require('lyaml')
local example_yaml = [[
- name: Yoda
  age: 900
- name: Luke Skywalker
  age: 53
]]

-- Parse YAML string
local people = yaml.load(example_yaml)

-- Iterate and print
for _, person in ipairs(people) do
  print(person.name .. " on " .. person.age .. " vuotta vanha.")
end
```

Sample Output:
```
Yoda on 900 vuotta vanha.
Luke Skywalker on 53 vuotta vanha.
```

## Deep Dive
"Sukellus syvälle:"

YAML (YAML Ain’t Markup Language) on kehitetty alun perin 2001. Se muistuttaa JSONia kerätessään tietoja avain-arvopareina, mutta on luettavuudeltaan parempi. Vaihtoehtoina ovat JSON ja XML, joista molemmat ovat yhtä koneellisesti käsiteltävissä mutta vähemmän ihmiskäytössä ystävällisiä. Työskennellessäsi YAML:n kanssa Luassa, `lyaml` on suosittu kirjasto, mutta muista asentaa se ensin esimerkiksi luarocks-paketinhallinnalla.

## See Also
"Katso myös:"

- YAML-virallinen sivusto: [https://yaml.org/](https://yaml.org/)
- LuaRocks lyaml-paketti: [https://luarocks.org/modules/gvvaughan/lyaml](https://luarocks.org/modules/gvvaughan/lyaml)
- YAML ja JSON vertailu: [https://json2yaml.com/](https://json2yaml.com/)
