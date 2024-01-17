---
title:                "Lavorare con yaml"
html_title:           "Ruby: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con YAML è un modo per organizzare e gestire dati in un formato leggibile sia per gli esseri umani che per i computer. I programmatori spesso usano YAML per salvare configurazioni o dati strutturati, e per importare e esportare informazioni da e verso un'applicazione.

## Come fare:
I codici seguenti mostrano come lavorare con YAML in Ruby utilizzando la gemma "yaml". Assicurati di avere la gemma installata nel tuo progetto Ruby.

```Ruby
require 'yaml'

# Creare un file YAML
yaml_file = {
  nome: 'Giulia',
  cognome: 'Rossi',
  età: 25
}

# Salvare il file YAML
File.open('dati.yaml', 'w') { |file| file.write(yaml_file.to_yaml) }

# Leggere il file YAML
leggi_file = YAML.load(File.read('dati.yaml'))

puts leggi_file[:nome] # Output: Giulia
puts leggi_file[:cognome] # Output: Rossi
puts leggi_file[:età] # Output: 25
```

## Approfondimento:
YAML (YAML Ain't Markup Language) è un formato di serializzazione di dati creato nel 2001 da Clark Evans e Ingy döt Net. È stato pensato per essere più semplice e leggibile rispetto ad altri formati come XML e JSON. Alcune alternative a YAML sono JSON, CSV e XML. La gemma "yaml" è inclusa nella libreria standard di Ruby, quindi non è necessario installarla separatamente.

## Vedi anche:
- [Documentazione YAML](https://yaml.org/) per ulteriori informazioni sul formato YAML.
- [Wiki di Ruby su YAML](https://github.com/ruby/ruby/blob/master/lib/yaml/README) per una documentazione più dettagliata sulla gemma "yaml".