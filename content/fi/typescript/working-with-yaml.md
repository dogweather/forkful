---
title:                "Työskentely yaml:n kanssa"
html_title:           "TypeScript: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML on tiedostomuoto, jota ohjelmoijat käyttävät konfiguraatiotiedostojen luomiseen ja tallentamiseen. Se on suosittu erityisesti DevOps- ja pilvialustojen maailmassa. YAML on helppo lukea ja kirjoittaa, joten se on kätevä vaihtoehto monimutkaisille JSON-tiedostoille.

## Miten:
```TypeScript
// Esimerkki YAML-tiedoston lukemisesta ja muuntamisesta JavaScript-objektiksi
import * as YAML from 'yaml';

const yamlData = `
name: John
age: 30
hobbies:
- hiking
- cooking
`;

const dataObject = YAML.parse(yamlData);
console.log(dataObject.age); // tulostaa "30"
```

## Syväsukellus:
YAML kehitettiin alun perin XML: n korvaajaksi helpottamaan tiedoston käsittelyä käyttöliittymäkirjastoille. Siitä on tullut suosittu vaihtoehto JSON-tiedostoille sen yksinkertaisuuden ja luettavuuden vuoksi. TypeScript-pohjaiset projektit voivat hyödyntää YAML-tukea esimerkiksi npm-paketin ```yaml``` avulla.

## Katso myös:
- [YAML-spesifikaatio](https://yaml.org/spec/)
- [JSON: sta YAML: ään muunnettu tieto](https://blog.formidable.com/from-json-to-yaml/)