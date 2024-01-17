---
title:                "Å jobbe med yaml"
html_title:           "TypeScript: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et tekstformat som brukes til å representere data strukturer på en enkel og lesbar måte. Programmere bruker YAML fordi det er enklere å lese og skrive sammenlignet med andre tekstformater som XML og JSON.

## Hvordan:
```TypeScript
import YAML from 'yaml';

// Definerer en YAML-streng
const yamlString: string = `
  bruker:
    navn: John
    alder: 30
    kjønn: Mann
`;

// Konverterer YAML-strengen til et JSON-objekt
const result = YAML.parse(yamlString);
console.log(result);
// Output: { bruker: { navn: 'John', alder: '30', kjønn: 'Mann' } }
```

## Dypdykk:
YAML står for "YAML Ain't Markup Language" og ble først publisert i 2001 av Clark Evans. Det er et enkelt og leselig format som er ideelt for å representere konfigurasjonsfiler og metadata. Alternativene til YAML inkluderer XML og JSON, men YAML er vanligvis foretrukket på grunn av sin enkelhet og lesbarhet. I TypeScript kan man bruke "yaml" pakken for å lese og skrive YAML-filer.

## Se også:
- [YAML offisiell hjemmeside](https://yaml.org/)
- [TypeScript offisiell hjemmeside](https://www.typescriptlang.org/)