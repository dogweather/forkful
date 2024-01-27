---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per codificare dati leggibili dall'uomo. Programmatori lo usano per configurazioni, perché è semplice e chiaro.

## How to:
Ecco come leggere e scrivere YAML in Bash:

```Bash
# Install yq (Un tool per YAML basato su jq)
wget https://github.com/mikefarah/yq/releases/download/v4.6.1/yq_linux_amd64 -O /usr/bin/yq && chmod +x /usr/bin/yq

# Scrivi YAML
cat <<EOT > config.yaml
hostname: example.com
port: 8080
EOT

# Leggi YAML
value=$(yq e '.hostname' config.yaml)
echo $value  # Output: example.com
```

## Deep Dive
YAML, acronimo di "YAML Ain't Markup Language", è stato introdotto nel 2001. Alternative includono JSON e XML. In Bash, comuni tool per lavorare con YAML sono `yq` e `jq` (per JSON). Dettagli di implementazione di `yq` si basano su libyaml per l'elaborazione efficiente e standardizzata.

## See Also
- Documentazione YAML: https://yaml.org/spec/1.2/spec.html
- GitHub `yq`: https://github.com/mikefarah/yq
- `jq` tutorial: https://stedolan.github.io/jq/tutorial/
