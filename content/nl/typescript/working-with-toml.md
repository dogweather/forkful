---
title:                "Werken met TOML"
date:                  2024-01-28T22:11:14.517288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML, een afkorting voor Tom's Obvious, Minimal Language, is een data-serialisatieformaat vergelijkbaar met JSON of YAML. Programmeurs gebruiken het vanwege de menselijke leesbaarheid en de eenvoudige koppeling aan datatypes, waardoor het een voor de hand liggende keuze is voor configuratiebestanden en gegevensuitwisseling.

## Hoe te:
Eerst heb je een TOML-parser nodig. `@iarna/toml` is een populaire keuze. Installeer het met npm: `npm install @iarna/toml --save`. Hier lees je een TOML-bestand en parseer je het naar een JavaScript-object:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Als `config.toml` bevat:
```
[server]
port = 8080
```
Dan zou de output zijn:
```
{ server: { port: 8080 } }
```
En, naar een TOML-bestand schrijven is net zo eenvoudig:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Het uitvoeren van deze code schrijft het object naar `config.toml` in TOML-formaat.

## Diepere Duik
TOML is gecreëerd door Tom Preston-Werner, de mede-oprichter van GitHub, rond 2013 als reactie op de beperkingen die hij waarnam in andere formaten zoals INI of YAML. Het is ontworpen om ondubbelzinnig en gemakkelijk te parsen naar datastructuren te zijn, en daarom, een favoriet voor configuratiebestanden. Alternatieven zoals JSON missen opmerkingen, terwijl YAML complexer is. TOML blinkt uit in zijn eenvoud en zijn vermogen om complexe gegevenshiërarchieën duidelijk te vertegenwoordigen.

Onder de motorkap, wanneer je TOML in TypeScript parsed, ben je tekstuele gegevens aan het omzetten in een gestructureerd formaat dat de taal kan manipuleren. Dit omvat lexen (ruwe tekst omzetten in tokens) en parsen (een interne datastructuur opbouwen); `@iarna/toml` handelt beide naadloos af. De ondersteuning van emoji's is een leuke touch, en toont TOML's gebruikersgerichte aanpak.

## Zie Ook
- Officiële TOML-specificatie: https://toml.io/en/
- `@iarna/toml`-pakket: https://www.npmjs.com/package/@iarna/toml
- Vergelijkingen tussen TOML, YAML en JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
