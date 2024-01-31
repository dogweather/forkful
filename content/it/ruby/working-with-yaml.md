---
title:                "Lavorare con YAML"
date:                  2024-01-19
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per salvare oggetti dati che sacrifica la verbosità per la leggibilità. I programmatori lo usano per configurazioni, dumping di dati e scambio di messaggi, in quanto è facile da leggere e scrivere umanamente.

## How to:
Per lavorare con YAML in Ruby, avrai bisogno della gemma 'yaml'. Ecco un esempio di come convertire un hash Ruby in una stringa YAML e viceversa.

```Ruby
require 'yaml'

# Convertiamo un Ruby Hash in una stringa YAML
data = {name: "Mario Rossi", profession: "Sviluppatore"}
yaml_string = data.to_yaml
puts yaml_string

# Convertire una stringa YAML in un Ruby object (Hash)
yaml_loaded = YAML.load(yaml_string)
puts yaml_loaded
```

Output:
```YAML
---
:name: Mario Rossi
:profession: Sviluppatore
```

```Ruby
{"name"=>"Mario Rossi", "profession"=>"Sviluppatore"}
```

## Deep Dive
YAML, acronimo di "YAML Ain't Markup Language", è stato proposto nel 2001 come alternativa a XML. Leggibile dall'uomo ma anche facilmente parsabile dai computer, YAML trova un equilibrio tra i due. Altre opzioni includono JSON e TOML, ma YAML è spesso preferito in ambienti di sviluppo per la sua semplicità, benché TOML stia guadagnando popolarità in strumenti come Cargo per Rust. Implementare YAML in Ruby è diretto grazie alla gemma 'yaml' che sfrutta 'Psych', il processor YAML incluso di default con Ruby.

## See Also
- Documentazione YAML ufficiale: [https://yaml.org/](https://yaml.org/)
- YAML su Wikipedia: [https://it.wikipedia.org/wiki/YAML](https://it.wikipedia.org/wiki/YAML)
- TOML su GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
