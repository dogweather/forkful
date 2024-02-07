---
title:                "Lavorare con YAML"
date:                  2024-02-03T19:26:01.101883-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

YAML, acronimo di "YAML Ain't Markup Language" (YAML non è un linguaggio di marcatura), è uno standard di serializzazione di dati leggibile dall'uomo che viene spesso utilizzato per file di configurazione e scambio di dati tra linguaggi. I programmatori sfruttano YAML per la sua semplicità e leggibilità, rendendolo la scelta preferita per impostazioni, configurazioni di applicazioni diverse o contenuti che dovrebbero essere modificabili da non programmatori.

## Come fare:

Lua non ha un supporto integrato per YAML, ma puoi lavorare con file YAML utilizzando librerie di terze parti come `lyaml`. Questa libreria permette la codifica e decodifica di dati YAML con Lua. Prima di tutto, dovrai installare `lyaml` tramite LuaRocks, il gestore di pacchetti di Lua:

```bash
luarocks install lyaml
```

### Decodifica YAML:

Supponiamo che tu abbia il seguente contenuto YAML in un file chiamato `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

Puoi decodificare questo file YAML in una tabella Lua con il seguente codice:

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

Quando esegui questo script, dovrebbe produrre:

```output
host: localhost
port: 3306
username: user
password: pass
```

### Codifica YAML:

Per codificare tabelle Lua in formato YAML, utilizzi la funzione `dump` fornita da `lyaml`. Considerando che vuoi creare una rappresentazione YAML della seguente tabella Lua:

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

L'output YAML sarà:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

Seguendo questi schemi, i programmatori Lua possono gestire efficacemente i dati YAML per una varietà di applicazioni. Queste operazioni con YAML sono fondamentali per sviluppare applicazioni Lua versatili che interagiscono senza problemi con altre parti di un sistema o direttamente con altri sistemi.
