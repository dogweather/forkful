---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:50.522295-07:00
description: "Hur man g\xF6r: I JavaScript inneb\xE4r arbete med YAML vanligtvis att\
  \ man anv\xE4nder ett tredjepartsbibliotek eftersom spr\xE5ket inte inkluderar en\
  \ inbyggd parser\u2026"
lastmod: '2024-03-13T22:44:38.313621-06:00'
model: gpt-4-0125-preview
summary: "I JavaScript inneb\xE4r arbete med YAML vanligtvis att man anv\xE4nder ett\
  \ tredjepartsbibliotek eftersom spr\xE5ket inte inkluderar en inbyggd parser f\xF6\
  r YAML."
title: Att Arbeta med YAML
weight: 41
---

## Hur man gör:
I JavaScript innebär arbete med YAML vanligtvis att man använder ett tredjepartsbibliotek eftersom språket inte inkluderar en inbyggd parser för YAML. Ett av de mest populära biblioteken för detta ändamål är `js-yaml`. Du kan använda `js-yaml` för att tolka YAML till JavaScript-objekt och tvärtom.

Först måste du installera `js-yaml`:

```bash
npm install js-yaml
```

Sedan kan du använda det i dina projekt. Så här kan du läsa in en YAML-fil och tolka den till ett JavaScript-objekt:

```javascript
// Kräv js-yaml-modulen
const yaml = require('js-yaml');
const fs   = require('fs');

// Läs in YAML från en fil
försök {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} fånga (e) {
  console.error(e);
}
```

Om din `config.yaml`-fil ser ut så här:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Kommer utdata att vara:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

För att göra tvärtom, konvertera ett JavaScript-objekt till en YAML-sträng:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Den här koden kommer att producera:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Genom att använda `js-yaml` kan du enkelt integrera YAML-tolkning och serialisering i dina JavaScript-projekt, vilket förbättrar datatutbytbarheten och konfigurationshanteringen.
