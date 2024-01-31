---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML, "YAML Ain't Markup Language", er et dataformat for å strukturere data. Programmere bruker YAML for enkel lesbarhet og skrivbarhet i konfigurasjonsfiler og datautveksling.

## Hvordan:
Lua har ikke innebygget YAML-støtte, så vi bruker `lyaml` biblioteket. Installer via luarocks:

```bash
luarocks install lyaml
```

Enkelt eksempel for å laste inn YAML-streng:
```lua
local lyaml = require('lyaml')
local yaml_data = [[
- Hanne
- Ole
- Kari
]]

local table_from_yaml = lyaml.load(yaml_data)
print(table_from_yaml[2])  -- Output: Ole
```

For å serialisere table til YAML:
```lua
local table_to_yaml = lyaml.dump({[1] = "Hanne", [2] = "Ole", [3] = "Kari"})
print(table_to_yaml)
```

## Dypdykk
YAML startet i 2001, en enklere og mer menneske-vennlig alternativ til XML og JSON. Alternativer til YAML er JSON for datautveksling eller TOML for konfigurasjon. Detaljene i YAML-implementeringen avhenger av biblioteket, men det følger standarden YAML 1.2.

## Se Også
- Offisiell YAML-nettside: https://yaml.org
- `lyaml` dokumentasjon: http://gvvaughan.github.io/lyaml/
- YAML 1.2 spesifikasjon: https://yaml.org/spec/1.2/spec.html
