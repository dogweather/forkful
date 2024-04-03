---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:45.960501-07:00
description: "YAML ain't Markup Language, of YAML, is een gebruiksvriendelijke standaard\
  \ voor gegevensserialisatie voor alle programmeertalen. Programmeurs werken met\u2026"
lastmod: '2024-03-13T22:44:51.223172-06:00'
model: gpt-4-0125-preview
summary: YAML ain't Markup Language, of YAML, is een gebruiksvriendelijke standaard
  voor gegevensserialisatie voor alle programmeertalen.
title: Werken met YAML
weight: 41
---

## Wat & Waarom?
YAML ain't Markup Language, of YAML, is een gebruiksvriendelijke standaard voor gegevensserialisatie voor alle programmeertalen. Programmeurs werken met YAML omdat het gemakkelijk te lezen en te schrijven is, vaak gebruikt wordt voor configuratiebestanden, en voor gegevensuitwisseling tussen talen of diensten.

## Hoe te:
We zullen de populaire `js-yaml` bibliotheek gebruiken om YAML te parseren naar JavaScript-objecten en JavaScript-objecten te converteren naar YAML.

1. Installeer eerst de bibliotheek:

```bash
npm install js-yaml
```

2. Parseer YAML naar JavaScript:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

probeer {
  const doc = yaml.load(fs.readFileSync('config.yml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Voorbeelduitvoer als `config.yml` is:

```yaml
version: 1
services:
  - webapp
  - database
```

Kan er als volgt uitzien:

```javascript
{ version: 1, services: [ 'webapp', 'database' ] }
```

3. Zet JavaScript om naar YAML:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

let data = {
  title: "YAML Voorbeeld",
  description: "YAML is makkelijk"
};

probeer {
  const ymlText = yaml.dump(data);
  fs.writeFileSync('example.yml', ymlText, 'utf8');
} catch (e) {
  console.error(e);
}
```

Dit zal een bestand `example.yml` creëren met:

```yaml
title: YAML Voorbeeld
description: 'YAML is makkelijk'
```

## Diepgaande Duik
YAML is gestart in 2001, ontworpen om gemakkelijk leesbaar te zijn voor mensen en comfortabel met de hand te schrijven. Alternatieven als JSON en XML zijn er wel, maar zijn niet zo eenvoudig voor mensen. De eenvoud van YAML kan leiden tot beveiligingsproblemen als het niet correct geïmplementeerd is, zoals het uitschakelen van `!!python/object/apply` om willekeurige code-uitvoering te voorkomen. Bibliotheken zoals `js-yaml` bieden opties om het parseren en omzetten van YAML aan te passen om beveiliging en functionaliteit toe te voegen.

## Zie Ook
- YAML 1.2 specificatie: https://yaml.org/spec/1.2/spec.html
- js-yaml GitHub repo: https://github.com/nodeca/js-yaml
- YAML Wikipedia-pagina voor meer achtergrondinformatie: https://nl.wikipedia.org/wiki/YAML
- Vergelijking tussen JSON en YAML: https://phoenixnap.com/kb/yaml-vs-json
