---
title:                "Arbeider med YAML"
aliases: - /no/lua/working-with-yaml.md
date:                  2024-02-03T19:26:09.343791-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

YAML, forkortet fra "YAML Ain't Markup Language", er en menneskelesbar data-serialiseringsstandard som ofte brukes for konfigurasjonsfiler og datautveksling mellom språk. Programmerere benytter YAML på grunn av dets enkelhet og lesbarhet, noe som gjør det til et foretrukket valg for innstillinger, varierte applikasjonskonfigurasjoner, eller innhold som skal kunne redigeres av ikke-programmerere.

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
