---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:09.343791-07:00
description: "Hvordan: Lua har ikke innebygd st\xF8tte for YAML, men du kan arbeide\
  \ med YAML-filer ved \xE5 bruke tredjeparts biblioteker som `lyaml`. Dette biblioteket\u2026"
lastmod: '2024-03-13T22:44:40.950671-06:00'
model: gpt-4-0125-preview
summary: "Lua har ikke innebygd st\xF8tte for YAML, men du kan arbeide med YAML-filer\
  \ ved \xE5 bruke tredjeparts biblioteker som `lyaml`."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Lua har ikke innebygd støtte for YAML, men du kan arbeide med YAML-filer ved å bruke tredjeparts biblioteker som `lyaml`. Dette biblioteket tillater koding og dekoding av YAML-data med Lua. Først må du installere `lyaml` via LuaRocks, Lua sin pakkehåndterer:

```bash
luarocks install lyaml
```

### Dekoding av YAML:
Anta at du har følgende YAML-innhold i en fil kalt `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  brukernavn: bruker
  passord: pass
```

Du kan dekode denne YAML-filen til et Lua-tabell med følgende kode:

```lua
local yaml = require('lyaml')
local fil = io.open("config.yaml", "r")
local innhold = fil:read("*all")
fil:close()

local data = yaml.load(innhold)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Når du kjører dette skriptet, skal det gi følgende utskrift:

```output
host: localhost
port: 3306
brukernavn: bruker
passord: pass
```

### Koding av YAML:
For å kode Lua-tabeller til YAML-format, bruker du `dump`-funksjonen som tilbys av `lyaml`. Med tanke på at du ønsker å lage en YAML-representasjon av følgende Lua-tabell:

```lua
local data = {
  nettside = {
    navn = "Eksempel",
    eier = "Jane Doe",
    metadata = {
      opprettelsesdato = "2023-01-01",
      tagger = {"blogg", "personlig", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

Det YAML-utgitte vil være:

```yaml
- nettside:
    metadata:
      opprettelsesdato: '2023-01-01'
      tagger: [blogg, personlig, lua]
    navn: Eksempel
    eier: Jane Doe
```

Ved å følge disse mønstrene, kan Lua-programmerere effektivt håndtere YAML-data for en rekke applikasjoner. Disse operasjonene med YAML er avgjørende for å utvikle fleksible Lua-applikasjoner som interagerer jevnt med andre deler av et system eller direkte med andre systemer.
