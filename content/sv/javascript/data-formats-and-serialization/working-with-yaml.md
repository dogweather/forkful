---
aliases:
- /sv/javascript/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:50.522295-07:00
description: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r ett l\xE4\
  sbarhetsv\xE4nligt data-serialiseringsformat. Programmerare anv\xE4nder ofta detta\
  \ f\xF6r\u2026"
lastmod: 2024-02-18 23:08:52.184475
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r ett l\xE4sbarhetsv\xE4\
  nligt data-serialiseringsformat. Programmerare anv\xE4nder ofta detta f\xF6r\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för YAML Ain't Markup Language, är ett läsbarhetsvänligt data-serialiseringsformat. Programmerare använder ofta detta för konfigurationsfiler och datautbyte mellan språk på grund av dess enkelhet och läsbarhet jämfört med JSON eller XML.

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
