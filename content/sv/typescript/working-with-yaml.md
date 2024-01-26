---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML är ett dataformat för att beskriva konfigurationer och data. Programmerare använder det för dess läsbarhet och enkelhet i applikationer och automatiserade skript.

## How to:
För att hantera YAML i TypeScript, behöver vi ett bibliotek som `js-yaml`. Installera det med npm:

```bash
npm install js-yaml
```

Sedan kan vi importera och använda `js-yaml` för att läsa och skriva YAML.

### Läs en YAML-fil:

```TypeScript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

const yamlContent = fs.readFileSync('config.yaml', 'utf8');
const data = yaml.load(yamlContent);
console.log(data);
```

### Skriv till en YAML-fil:

```TypeScript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

const newData = { name: 'Programmeraren', language: 'TypeScript' };
const newYamlContent = yaml.dump(newData);
fs.writeFileSync('newConfig.yaml', newYamlContent, 'utf8');
```

## Deep Dive
YAML, "YAML Ain't Markup Language", skapades 2001 som ett mer läsbart alternativ till XML och JSON. Om JSON upplevs för klumpig, är YAML ett bra alternativ, speciellt för konfigurationsfiler. Byggt för att mappa till datatyper i flera programmeringsspråk, är det ofta använd i DevOps för containers och orkestrering, som Docker och Kubernetes.

YAML-hantering i TypeScript sker i huvudsak genom bibliotek. `js-yaml` är populärt, men det finns andra som `yaml` och `yamljs`. Valet beror på projektets behov och utvecklarens preferenser.

## See Also
- YAML specifikation: https://yaml.org/spec/
- js-yaml GitHub-sida: https://github.com/nodeca/js-yaml
- YAML och TypeScript-guide: https://www.typescriptlang.org/docs/handbook/yaml-and-json.html
