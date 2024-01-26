---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un linguaggio per serializzare dati leggibile dall'uomo. I programmatori lo usano per configurazioni di progetti, file di Docker, e per facilitare lo scambio dati tra linguaggi diversi.

## How to:
Installiamo `PyYAML`, una libreria Python per lavorare con file YAML:
```bash
pip install PyYAML
```

Leggere un file YAML:
```Python
import yaml

# Supponendo che 'config.yaml' sia il nostro file
with open('config.yaml', 'r') as file_stream:
    data = yaml.safe_load(file_stream)

print(data)
```

Scrivere un file YAML:
```Python
import yaml

data = {'key': 'value', 'numbers': [1, 2, 3]}

with open('output.yaml', 'w') as file_stream:
    yaml.dump(data, file_stream)
```

## Deep Dive:
YAML (YAML Ain't Markup Language) è nato all'inizio degli anni 2000 come alternativa a XML. Formati alternativi includono JSON e TOML. YAML si distingue per la priorità alla leggibilità umana e supporta tipi di dati complessi. In Python, si fa affidamento a `PyYAML` per operazioni standard di parsing e scrittura. Tieni presente che è importante usare `safe_load()` invece di `load()` per evitare l'esecuzione di codice malevolo.

## See Also:
Approfondisci su YAML e `PyYAML` qui:
- Documentazione di YAML: https://yaml.org/spec/1.2/spec.html
- Repository GitHub di PyYAML: https://github.com/yaml/pyyaml
- Guida su YAML in Python: https://realpython.com/python-yaml/
