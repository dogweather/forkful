---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et enkelt tekstformat for datastrukturer. Programmerere bruker det for konfigurasjonsfiler, datautveksling og lagring av objekter for en rekke språk på grunn av lesbarheten og enkelheten.

## Hvordan:
For å manipulere YAML-filer i Fish Shell, kan du bruke kommandoer som `yq`. Her er et eksempel:

```Fish Shell
# Installer yq
sudo apt-get install yq

# Les en verdi fra YAML
yq r '.minVerdi' config.yaml

# Output:
# 1.0

# Oppdater en verdi i YAML
yq w -i config.yaml 'maxVerdi' 2.0

# Legg til en ny nøkkel og verdi
yq w -i config.yaml 'nyNøkkel' 'nyVerdi'
```

## Deep Dive:
YAML (YAML Ain't Markup Language) ble først introdusert i 2001, som et menneskelesbart alternativ til XML. I Fish Shell og mange andre shell-miljøer, er `yq`, basert på `jq` for JSON, det go-to-verktøyet for å håndtere YAML. Det er andre alternativer som `pyyaml` for de som foretrekker Python, men `yq` skinner for sin bash-integrasjon og enkel syntaks.

## Se Også:
- Det offisielle YAML-nettstedet: [https://yaml.org](https://yaml.org)
- `yq` GitHub-siden: [https://github.com/kislyuk/yq](https://github.com/kislyuk/yq)
- YAML-spesifikasjonen: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
