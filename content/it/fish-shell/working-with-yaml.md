---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato di dati facilmente leggibile utilizzato per la configurazione di file. I programmatori lo usano per la sua semplicità e leggibilità, rispetto ad altri formati come XML o JSON.

## How to:
Per lavorare con YAML in Fish, ti servono strumenti come 'yq'. Ecco un esempio di come leggere e modificare un file YAML.

```Fish Shell
# Leggi un valore da YAML
yq e '.database.host' config.yaml

# Modifica un valore e salva il file
yq e '.database.host = "localhost"' -i config.yaml
```

```Sample Output
server.example.com # Output dal leggere il valore
# Nessun output visibile per la modifica; il file config.yaml è aggiornato
```

## Deep Dive
YAML viene da "YAML Ain't Markup Language", sottolineando il suo focus sulla leggibilità. Alternativi includono JSON e TOML. In Fish, 'yq' gira sui binari di 'libyaml', il quale implementa YAML 1.1 e 1.2.

## See Also
- Documentazione di 'yq': https://mikefarah.gitbook.io/yq/
- YAML Specification: https://yaml.org/spec/1.2/spec.html
- Confronto tra formati di configurazione: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
