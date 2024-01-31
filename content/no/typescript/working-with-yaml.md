---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML er et menneskelesbart dataformat brukt til konfigurasjon og datautveksling. Programmerere bruker YAML fordi det er enkelt å skrive og lese, og passer godt med mange programmeringsspråk inkludert TypeScript.

## How to:
TypeScript kan jobbe med YAML ved å bruke et pakkebibliotek som `js-yaml`. Her er hvordan du leser og skriver YAML:

```typescript
import * as yaml from 'js-yaml';
import * as fs from 'fs';

// Lese YAML fra en fil
const doc = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
console.log(doc);

// Skrive et JavaScript-objekt til YAML
const data = { title: 'YAML Article', readers: ['Norwegian'] };
fs.writeFileSync('output.yaml', yaml.dump(data));

// output.yaml filen inneholder nå:
// title: YAML Article
// readers:
//   - Norwegian
```

## Deep Dive
YAML (YAML Ain't Markup Language) har eksistert siden 2001 og er et superset av JSON. Alternativer som JSON og XML brukes også, men YAMLs minimalistiske natur gir klarhet. Under kjøring konverterer TypeScript biblioteker som `js-yaml` YAML til JavaScript-objekter, hvilket tillater en sømløs integrasjon i prosjekter.

## See Also
- YAML offisiell side: https://yaml.org
- `js-yaml` GitHub-repositorium: https://github.com/nodeca/js-yaml
- TypeScript offisiell dokumentasjon: https://www.typescriptlang.org/docs/
