---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:49.071826-07:00
description: "YAML is een gebruikersvriendelijke data-serialisatiestandaard. Programmeurs\
  \ gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen en\u2026"
lastmod: '2024-03-13T22:44:50.571981-06:00'
model: gpt-4-0125-preview
summary: "YAML is een gebruikersvriendelijke data-serialisatiestandaard. Programmeurs\
  \ gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen en\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML is een gebruikersvriendelijke data-serialisatiestandaard. Programmeurs gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen en meer omdat het eenvoudig en leesbaar is.

## Hoe:

Om met YAML in TypeScript te werken, heb je een bibliotheek zoals `js-yaml` nodig. Installeer het eerst:

```bash
npm install js-yaml
```

Nu, parseer een YAML string naar een JavaScript object:

```typescript
import yaml from 'js-yaml';

const yamlStr = `
name: John Doe
age: 30
`;

probeer {
  const doc = yaml.load(yamlStr);
  console.log(doc);
} vang (e) {
  console.error(e);
}
```

Voorbeelduitvoer:

```json
{ name: 'John Doe', age: 30 }
```

Om een object naar een YAML string te converteren:

```typescript
import yaml from 'js-yaml';

const obj = { name: 'Jane Doe', age: 25 };

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Voorbeelduitvoer:

```yaml
name: Jane Doe
age: 25
```

## Diepgaande Duik

YAML is gestart in 2001, met als doel menselijke leesbaarheid en gegevensuitwisseling tussen talen. Het is een superset van JSON. Alternatieven omvatten JSON en XML, maar de minimale syntaxis van YAML wordt vaak de voorkeur gegeven voor configuratiebestanden. Als je met YAML in TypeScript werkt, vergeet dan niet dat het niet-getypeerd is; wees voorzichtig met de ontvangen gegevens, vooral van onbetrouwbare bronnen, om beveiligingsproblemen te vermijden.

## Zie Ook

- Officiële YAML-website: http://yaml.org
- `js-yaml` GitHub repo: https://github.com/nodeca/js-yaml
- YAML vs. JSON vergelijking: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
