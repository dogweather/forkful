---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"## Mikä & Miksi?"
YAML on datan serialisointikieli, jota käytetään konfiguraatiotiedostoihin ja datan tallennukseen. Käytämme YAMLia sen selkeyden ja ihmislukuisuuden vuoksi.

## How to:
"## Kuinka:"
```TypeScript
// Asenna yaml-kirjasto: npm install js-yaml
import * as yaml from 'js-yaml';
import * as fs from 'fs';

// Lukee YAML-tiedoston ja muuntaa sen JavaScript-objektiksi
const doc = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
console.log(doc);

// Muuntaa JavaScript-objektin YAML-muotoon ja tallentaa tiedostoon
const data = { title: 'Esimerkki', status: 'toimii' };
fs.writeFileSync('output.yaml', yaml.dump(data));
```
*Tuloste*:
Konsoli näyttää `config.yaml` sisällön JS-objektina.
`output.yaml` sisältää muunnetun datan YAML-muodossa.

## Deep Dive
"## Syväsukellus"
YAML (YAML Ain't Markup Language) on helppolukuinen datan kuvauskieli, joka ilmestyi 2001. JSON ja XML ovat vaihtoehtoisia kieliä, mutta YAML on suosittu ihmislukuisuuden ja kirjoittamisen helppouden vuoksi. YAMLin käyttö TypeScriptissa vaatii yleensä ulkopuolisen kirjaston, kuten `js-yaml`, jolla voi lukea ja kirjoittaa YAML-tietoa.

## See Also
"## Katso Myös"
- YAML:n virallinen sivusto: [https://yaml.org](https://yaml.org)
- `js-yaml` GitHub-repositorio: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- TypeScriptin dokumentaatio: [https://www.typescriptlang.org](https://www.typescriptlang.org)