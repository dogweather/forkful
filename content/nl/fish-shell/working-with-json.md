---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:17.994422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON (JavaScript Object Notation) is een gegevensindeling die wordt gebruikt voor het representeren van gestructureerde data. Programmeurs gebruiken JSON omdat het makkelijk te lezen en te schrijven is voor mensen, en eenvoudig te parsen en te genereren voor computers.

## Hoe te:

```Fish Shell
# JSON parsen vanuit een string met `jq`
echo '{"name": "Fish", "type": "Shell"}' | jq '.'

# Waarde van een specifieke sleutel krijgen
echo '{"name": "Fish", "type": "Shell"}' | jq '.name'

# Output:
# "Fish"

# Een waarde updaten en nieuwe JSON string uitvoeren
echo '{"name": "Fish", "type": "Shell"}' | jq '.type = "Command Line Interface"'

# JSON pretty-printen vanuit een bestand
cat config.json | jq '.'
```

## Diepere Duik

JSON, gestandaardiseerd in de vroege jaren 2000, vindt zijn oorsprong in de objectliteratuur van JavaScript. Het verving snel XML voor vele taken vanwege zijn lichte syntax en directe mapping naar datastructuren. Er bestaan alternatieven zoals YAML en TOML, maar de alomtegenwoordigheid van JSON maakt het een standaardkeuze in veel scenario's. Werken met JSON in Fish vereist hulpmiddelen zoals `jq`, omdat Fish zelf niet is ontworpen voor zware datamanipulatie. Historisch gezien gebruiken Unix-shells externe hulpmiddelen voor specifieke taken, en Fish volgt deze filosofie.

## Zie Ook

- De `jq` Handleiding: https://stedolan.github.io/jq/manual/
- Fish Shell Documentatie: https://fishshell.com/docs/current/index.html
- JSON Specificatie: https://www.json.org/json-en.html
