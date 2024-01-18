---
title:                "Lavorare con yaml"
html_title:           "Lua: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con YAML significa creare e gestire file di dati strutturati in modo leggibile sia per i programmatori che per i computer. I programmatori utilizzano YAML per memorizzare e trasmettere informazioni, come per esempio i dati di configurazione di un'applicazione o i contenuti di un sito web.

## Come fare:
Di seguito sono riportati alcuni esempi di codice in Lua per lavorare con YAML. 
```Lua
-- Importa il modulo YAML
local yaml = require 'yaml'

-- Carica un file YAML
local my_data = yaml.load("config.yml")

-- Accedi ai dati
print(my_data.app_name) -- Stampa "Mia App"
print(my_data.database.host) -- Stampa "localhost"

-- Converti una tabella Lua in YAML
local other_data = {
   name = "John",
   age = 32,
   hobbies = {"golf", "travel", "reading"}
}

local yaml_other_data = yaml.dump(other_data)
print(yaml_other_data) -- Stampa "name: John\nage: 32\nhobbies:\n- golf\n- travel\n- reading"
```

## Approfondimento:
YAML (YAML Ain't Markup Language) è un formato di serializzazione dei dati basato su testo. È stato creato nel 2001 per risolvere le limitazioni e la complessità di altri formati come XML. Altri formati di dati strutturati comunemente utilizzati sono JSON e XML, ma YAML è preferito per la sua leggibilità e facilità di utilizzo. Il supporto per YAML è presente in molti linguaggi di programmazione, inclusa la Lua.

## Vedi anche:
- Documentazione ufficiale di YAML: https://yaml.org/
- Libreria Lua per lavorare con YAML: https://github.com/GiuseppeLua/lua-yaml
- Altro confronto tra formati di dati strutturati: https://www.hackerearth.com/practice/notes/yaml-vs-json-vs-xml/