---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:04.976972-07:00
description: "YAML, lyhennys sanoista \"YAML Ain't Markup Language\", on ihmisen luettavissa\
  \ oleva datan serialisointistandardi, jota k\xE4ytet\xE4\xE4n usein asetustiedostoissa\u2026"
lastmod: '2024-03-13T22:44:56.716796-06:00'
model: gpt-4-0125-preview
summary: "YAML, lyhennys sanoista \"YAML Ain't Markup Language\", on ihmisen luettavissa\
  \ oleva datan serialisointistandardi, jota k\xE4ytet\xE4\xE4n usein asetustiedostoissa\u2026"
title: "Ty\xF6skentely YAML:n kanssa"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

YAML, lyhennys sanoista "YAML Ain't Markup Language", on ihmisen luettavissa oleva datan serialisointistandardi, jota käytetään usein asetustiedostoissa sekä datan vaihdossa eri kielten välillä. Ohjelmoijat suosivat YAMLia sen yksinkertaisuuden ja luettavuuden vuoksi, mikä tekee siitä ensisijaisen valinnan asetuksille, monipuolisille sovelluskonfiguraatioille tai sisällölle, jota ei-ohjelmoijien tulisi voida muokata.

## Miten:

Lualla ei ole sisäänrakennettua tukea YAMLille, mutta voit työskennellä YAML-tiedostojen kanssa käyttämällä kolmannen osapuolen kirjastoja, kuten `lyaml`. Tämä kirjasto mahdollistaa YAML-datan koodaamisen ja dekoodaamisen Lualla. Ensimmäiseksi sinun täytyy asentaa `lyaml` LuaRocksin, Luann paketinhallintajärjestelmän, kautta:

```bash
luarocks install lyaml
```

### YAML:n dekoodaus:

Oletetaan, että sinulla on seuraava YAML-sisältö tiedostossa nimeltä `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

Voit dekoodata tämän YAML-tiedoston Lua-taulukoksi seuraavalla koodilla:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Kun ajat tämän skriptin, sen tulisi tulostaa:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAML:n koodaus:

Lua-taulukoiden koodaamiseksi YAML-muotoon käytät `lyaml`-kirjaston tarjoamaa `dump`-funktiota. Oletetaan, että haluat luoda YAML-esityksen seuraavasta Lua-taulukosta:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

Tulostettu YAML olisi:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

Näiden mallien mukaisesti Lua-ohjelmoijat voivat tehokkaasti käsitellä YAML-dataa monenlaisissa sovelluksissa. Nämä toimenpiteet YAMLin kanssa ovat olennaisia monipuolisten Lua-sovellusten kehittämiselle, jotka toimivat sujuvasti osana suurempaa järjestelmää tai suoraan muiden järjestelmien kanssa.
