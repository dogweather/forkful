---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:57.944350-07:00
description: "Hvordan: \xC5 arbeide med YAML i TypeScript inneb\xE6rer vanligvis \xE5\
  \ parse YAML-innhold til JavaScript-objekter og muligens konvertere JavaScript-objekter\u2026"
lastmod: '2024-03-13T22:44:40.551966-06:00'
model: gpt-4-0125-preview
summary: "\xC5 arbeide med YAML i TypeScript inneb\xE6rer vanligvis \xE5 parse YAML-innhold\
  \ til JavaScript-objekter og muligens konvertere JavaScript-objekter tilbake til\
  \ YAML."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Å arbeide med YAML i TypeScript innebærer vanligvis å parse YAML-innhold til JavaScript-objekter og muligens konvertere JavaScript-objekter tilbake til YAML. Dette krever en parser; et populært valg er `js-yaml`, et bibliotek som enkelt kan integreres i TypeScript-prosjekter.

### Installere js-yaml
Først, legg til `js-yaml` i prosjektet ditt:

```bash
npm install js-yaml
```

### Parse YAML til JavaScript-objekt
Tenk deg at du har en YAML-fil `config.yaml` med følgende innhold:

```yaml
database:
  vert: localhost
  port: 5432
  brukernavn: bruker
  passord: pass
```

Du kan lese og parse denne filen til et JavaScript-objekt som følger:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Last inn og parse YAML-filen
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Eksempel på utskrift:**

```json
{
  "database": {
    "vert": "localhost",
    "port": 5432,
    "brukernavn": "bruker",
    "passord": "pass"
  }
}
```

### Konvertere JavaScript-objekt til YAML
Hvis du trenger å gjøre det motsatte og konvertere et JavaScript-objekt til en YAML-streng, kan du bruke `js-yaml` slik:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  tittel: "Eksempel",
  er_publisert: true,
  forfatter: {
    navn: "Jane Doe",
    alder: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Eksempel på utskrift:**

```yaml
tittel: Eksempel
er_publisert: true
forfatter:
  navn: Jane Doe
  alder: 34
```

Dette utsnittet konverterer et JavaScript-objekt til en YAML-streng og skriver den ut. I praksis kan du skrive dette tilbake til en fil eller bruke den i andre deler av applikasjonen din.
