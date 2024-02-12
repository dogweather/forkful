---
title:                "Werken met YAML"
aliases:
- /nl/fish-shell/working-with-yaml/
date:                  2024-01-28T22:11:44.293893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat en Waarom?

YAML, "YAML Ain't Markup Language", is een gebruiksvriendelijke data-serialisatiestandaard die notatieel superieur is aan tabel- en opmaaktalen voor configuratiebestanden en gegevensuitwisseling. Programmeurs gebruiken het vanwege de eenvoud en leesbaarheid in configuratiebestanden, implementatiemanifesten en meer complexe datastructuren.

## Hoe:

### YAML-configuratie lezen
```Fish Shell
# Uitgaande van 'config.yaml' bevat:
# naam: Fishy
# beroep: Shell

set config (yaml2json < config.yaml | jq -r '.name, .occupation')
echo $config
# Uitvoer: Fishy Shell
```

### Schrijven naar YAML-bestand
```Fish Shell
# Gebruikmakend van 'yq', een draagbare command-line YAML-processor
echo -e "naam: Nemo\nkleur: Oranje" > fish.yaml

# Een nieuwe sleutel toevoegen
yq e '.vrienden += ["Dory"]' -i fish.yaml

cat fish.yaml
# Uitvoer:
# naam: Nemo
# kleur: Oranje
# vrienden:
# - Dory
```

## Diepgaande verkenning

YAML kwam begin jaren 2000 naar voren als een vereenvoudiging van XML en is sindsdien een standaard geworden voor configuratiebestanden in de software-industrie. De minimale syntaxis is zowel een zegen als een vloek—makkelijk te lezen maar lastig om te parsen zonder bibliotheken. Alternatieven voor YAML omvatten JSON, XML, en TOML, elk met hun eigen afwegingen in gebruikssituaties. In Fish Shell worden `yq` en `yaml2json` veel gebruikt voor het manipuleren van YAML-bestanden, aangezien Fish geen ingebouwde YAML-parser heeft.

## Zie ook

- Officiële YAML-site: https://yaml.org
- `jq` handleiding: https://stedolan.github.io/jq/manual/
- `yq` repository en documentatie: https://github.com/mikefarah/yq
