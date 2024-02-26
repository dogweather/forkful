---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:06.636785-07:00
description: "Werken met JSON in Bash houdt in dat er JSON-geformatteerde gegevens\
  \ direct vanaf de opdrachtregel worden geparseerd en gegenereerd. Programmeurs doen\
  \ dit\u2026"
lastmod: '2024-02-25T18:49:48.341982-07:00'
model: gpt-4-0125-preview
summary: "Werken met JSON in Bash houdt in dat er JSON-geformatteerde gegevens direct\
  \ vanaf de opdrachtregel worden geparseerd en gegenereerd. Programmeurs doen dit\u2026"
title: Werken met JSON
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON in Bash houdt in dat er JSON-geformatteerde gegevens direct vanaf de opdrachtregel worden geparseerd en gegenereerd. Programmeurs doen dit voor configuratiebeheer, API-interactie en gegevensuitwisseling tussen diensten vanwege de alomtegenwoordigheid van JSON over platforms en talen heen.

## Hoe te:

```Bash
# JSON parsen met 'jq':
echo '{"name": "John", "age": 31, "city": "New York"}' | jq '.name'
# Uitvoer: "John"

# JSON genereren met 'jq':
echo '{}' | jq --arg name "John" --arg city "New York" '. | .name=$name | .city=$city'
# Uitvoer: {"name":"John","city":"New York"}

# JSON-bestand lezen en gegevens extraheren:
jq '.users[] | select(.id == "123")' users.json
# Ervan uitgaande dat users.json de relevante gegevensstructuur bevat.
```

## Dieper Duiken

JSON (JavaScript Object Notation) werd in de vroege jaren 2000 geformaliseerd en werd snel een standaard voor gegevensuitwisseling. In een Bash-context kwam `jq` als een robuust hulpmiddel voor JSON-verwerking naar voren, dat een DSL (domeinspecifieke taal) biedt voor het bevragen en manipuleren van JSON-gegevens. Alternatieven zijn onder andere `jshon` en `jo`. Werken met JSON in Bash omvat typisch het gebruik van externe hulpmiddelen zoals deze, omdat Bash geen ingebouwde JSON-parseermogelijkheden heeft.

## Zie Ook

- `jq` Handleiding: https://stedolan.github.io/jq/manual/
- Wikipedia-artikel over JSON: https://nl.wikipedia.org/wiki/JSON
- Bash Scripting Gids: https://www.gnu.org/software/bash/manual/
