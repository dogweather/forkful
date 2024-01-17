---
title:                "Lavorare con YAML"
html_title:           "Python: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

YAML è un formato per dati strutturati basato su una sintassi in stile indentazione. I programmatori lo usano per facilitare la lettura e la scrittura di file di configurazione e dati complessi in modo leggibile sia per gli umani che per le macchine.

## Come:

```
import yaml

# Creazione di un dizionario
config = {"database": {"host": "localhost",
                        "user": "root",
                        "password": "secret123",
                        "database_name": "mydatabase"}}

# Scrittura del dizionario in formato YAML
with open("config.yml", "w") as f:
    yaml.dump(config, f)

# Lettura del file YAML
with open("config.yml") as f:
    config = yaml.load(f, Loader=yaml.FullLoader)

# Accesso ai valori 
print("Host del database:", config["database"]["host"])
```

Output:

```
Host del database: localhost
```

## Approfondimento:

YAML è stato sviluppato da Clark Evans nel 2001 come alternativa a formati come XML e JSON. Una delle sue caratteristiche più apprezzate è la possibilità di includere commenti all'interno dei file, rendendolo particolarmente adatto per la scrittura di documentazione. Esistono altri formati simili a YAML, come ad esempio TOML e HCL, ma YAML rimane uno dei più diffusi.

## Vedi anche:

- [Documentazione ufficiale di YAML](https://yaml.org/)
- [Tutorial su YAML in Python](https://realpython.com/python-yaml/)