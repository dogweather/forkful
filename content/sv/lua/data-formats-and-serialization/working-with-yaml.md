---
title:                "Att Arbeta med YAML"
aliases:
- /sv/lua/working-with-yaml/
date:                  2024-02-03T19:26:22.212337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, en förkortning för "YAML Ain't Markup Language," är en människoläsbar standard för serialisering av data som ofta används för konfigurationsfiler och datautbyte mellan språk. Programmerare använder YAML på grund av dess enkelhet och läsbarhet, vilket gör det till ett föredraget val för inställningar, diverse applikationskonfigurationer eller innehåll som ska kunna redigeras av icke-programmerare.

## Hur man gör:

Lua har inte inbyggt stöd för YAML, men du kan arbeta med YAML-filer genom att använda tredjepartsbibliotek såsom `lyaml`. Detta bibliotek möjliggör kodning och avkodning av YAML-data med Lua. Först behöver du installera `lyaml` via LuaRocks, Luas pakethanterare:

```bash
luarocks install lyaml
```

### Avkoda YAML:

Anta att du har följande YAML-innehåll i en fil som heter `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

Du kan avkoda denna YAML-fil till en Lua-tabell med följande kod:

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

När du kör detta skript bör det ge följande utskrift:

```output
host: localhost
port: 3306
username: user
password: pass
```

### Koda YAML:

För att koda Lua-tabeller till YAML-format använder du funktionen `dump` som tillhandahålls av `lyaml`. Antag att du vill skapa en YAML-representation av följande Lua-tabell:

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

Den resulterande YAML blir:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

Genom att följa dessa mönster kan Lua-programmerare effektivt hantera YAML-data för en mängd olika applikationer. Dessa operationer med YAML är avgörande för att utveckla flexibla Lua-applikationer som interagerar smidigt med andra delar av ett system eller direkt med andra system.
