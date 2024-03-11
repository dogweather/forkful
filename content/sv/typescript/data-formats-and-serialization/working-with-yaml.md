---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.748053-07:00
description: "YAML, ett dataserialiseringsspr\xE5k designat f\xF6r att vara anv\xE4\
  ndarv\xE4nligt, anv\xE4nds ofta f\xF6r konfigurationsfiler, mellanprocessmeddelanden\
  \ och datalagring.\u2026"
lastmod: '2024-03-11T00:14:11.015186-06:00'
model: gpt-4-0125-preview
summary: "YAML, ett dataserialiseringsspr\xE5k designat f\xF6r att vara anv\xE4ndarv\xE4\
  nligt, anv\xE4nds ofta f\xF6r konfigurationsfiler, mellanprocessmeddelanden och\
  \ datalagring.\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, ett dataserialiseringsspråk designat för att vara användarvänligt, används ofta för konfigurationsfiler, mellanprocessmeddelanden och datalagring. Programmerare lutar sig mot YAML för dess läsbarhet och användarvänlighet, särskilt när man hanterar komplex strukturerad data, vilket gör det till ett utmärkt val för applikationer utvecklade i TypeScript.

## Hur man gör:
Att arbeta med YAML i TypeScript innebär vanligtvis att tolka YAML-innehåll till JavaScript-objekt och möjligen konvertera JavaScript-objekt tillbaka till YAML. Detta kräver en tolk; ett populärt val är `js-yaml`, ett bibliotek som enkelt kan integreras i TypeScript-projekt.

### Installera js-yaml
Först, lägg till `js-yaml` i ditt projekt:

```bash
npm install js-yaml
```

### Tolka YAML till JavaScript-objekt
Föreställ dig att du har en YAML-fil `config.yaml` med följande innehåll:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Du kan läsa och tolka denna fil till ett JavaScript-objekt på följande sätt:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Ladda och tolka YAML-filen
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Exempelutskrift:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Konvertera JavaScript-objekt till YAML
Om du behöver gå åt andra hållet och konvertera ett JavaScript-objekt till en YAML-sträng kan du använda `js-yaml` på följande sätt:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Exempelutskrift:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Detta kodsnutt konverterar ett JavaScript-objekt till en YAML-sträng och skriver ut den. I praktiken kan du skriva tillbaka detta till en fil eller använda det i andra delar av din applikation.
