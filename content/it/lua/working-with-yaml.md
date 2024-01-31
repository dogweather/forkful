---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per dati leggibili dall'uomo, usato per la configurazione di applicazioni. I programmatori lo usano per la sua semplicità e facilità di lettura rispetto a JSON o XML.

## How to:
Non c'è supporto integrato per YAML in Lua, ma possiamo usare una libreria esterna. Installiamo `lyaml` con l'uso di luarocks:

```bash
luarocks install lyaml
```

Ecco come leggere un YAML:

```Lua
local lyaml = require 'lyaml'

local yaml_data = [[
- pippo
- pluto
- paperino
]]

local lua_table = lyaml.load(yaml_data)
for i, name in ipairs(lua_table) do
  print(i, name)
end

--[[
Output:
1	pippo
2	pluto
3	paperino
]]
```

Scriviamo in YAML:

```Lua
local lyaml = require 'lyaml'

local to_yaml_data = {
  "topolino",
  "minnie",
  "clarabella"
}

local yaml_string = lyaml.dump({to_yaml_data})
print(yaml_string)

--[[
Output:
- topolino
- minnie
- clarabella
]]
```

## Deep Dive
YAML, che sta per "YAML Ain't Markup Language" (gioco di parole "YAML non è un linguaggio di markup"), è nato nel 2001. Non è veloce come JSON per il parsing ma migliora la leggibilità. Altre librerie Lua per YAML includono `yaml` e `tinyyaml`. Implementare il parsing da soli è complesso a causa delle varie funzionalità di YAML, quindi usare una libreria è la scelta pratica.

## See Also
- Documentazione `lyaml`: http://gvvaughan.github.io/lyaml/
- YAML ufficiale: https://yaml.org/
- Introduzione a YAML per i programmatori Lua: https://learnxinyminutes.com/docs/yaml/
