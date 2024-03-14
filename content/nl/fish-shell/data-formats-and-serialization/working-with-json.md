---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.994422-07:00
description: "JSON (JavaScript Object Notation) is een gegevensindeling die wordt\
  \ gebruikt voor het representeren van gestructureerde data. Programmeurs gebruiken\
  \ JSON\u2026"
lastmod: '2024-03-13T22:44:51.268462-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) is een gegevensindeling die wordt gebruikt\
  \ voor het representeren van gestructureerde data. Programmeurs gebruiken JSON\u2026"
title: Werken met JSON
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
