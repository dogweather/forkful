---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.748053-07:00
description: "Hur man g\xF6r: Att arbeta med YAML i TypeScript inneb\xE4r vanligtvis\
  \ att tolka YAML-inneh\xE5ll till JavaScript-objekt och m\xF6jligen konvertera JavaScript-objekt\u2026"
lastmod: '2024-03-13T22:44:37.676289-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med YAML i TypeScript inneb\xE4r vanligtvis att tolka YAML-inneh\xE5\
  ll till JavaScript-objekt och m\xF6jligen konvertera JavaScript-objekt tillbaka\
  \ till YAML."
title: Att Arbeta med YAML
weight: 41
---

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
