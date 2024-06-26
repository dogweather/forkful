---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.994422-07:00
description: "Hoe te: JSON, gestandaardiseerd in de vroege jaren 2000, vindt zijn\
  \ oorsprong in de objectliteratuur van JavaScript. Het verving snel XML voor vele\
  \ taken\u2026"
lastmod: '2024-04-05T21:53:51.275873-06:00'
model: gpt-4-0125-preview
summary: JSON, gestandaardiseerd in de vroege jaren 2000, vindt zijn oorsprong in
  de objectliteratuur van JavaScript.
title: Werken met JSON
weight: 38
---

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
