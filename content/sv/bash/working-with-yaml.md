---
title:                "Arbete med YAML"
date:                  2024-01-19
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för dataseriering, perfekt för konfigurationsfiler. Programmerare använder det för dess läslighet och enkelhet.

## How to:
Exempel: Läs en YAML-fil i Bash.

```Bash
# Installera 'yq', ett verktyg för att hantera YAML-filer.
sudo wget https://github.com/mikefarah/yq/releases/download/v4.9.6/yq_linux_amd64 -O /usr/bin/yq && sudo chmod +x /usr/bin/yq

# Antag att 'config.yaml' innehåller:
# name: Elsa
# role: Programmer

# Läs värde från YAML-filen.
name=$(yq e '.name' config.yaml)
echo "Namn: $name"  # Skriver ut "Namn: Elsa"
```

## Utforskningen:
YAML, "YAML Ain't Markup Language", skapades 2001 som ett mer läsbart alternativ till XML. JSON och TOML är ofta använda alternativ. YAML är en favorit i DevOps tack vare dess kompatibilitet med komplexa strukturer, medan det bibehåller enkelheten i till exempel JSON.

## Se Också:
- YAML's officiella hemsida: [yaml.org](https://yaml.org)
- `yq` verktygets GitHub repo: [github.com/mikefarah/yq](https://github.com/mikefarah/yq)
