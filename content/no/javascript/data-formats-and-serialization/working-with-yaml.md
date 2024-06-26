---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:36.601474-07:00
description: "Hvordan: I JavaScript inneb\xE6rer arbeid med YAML vanligvis bruk av\
  \ et tredjeparts bibliotek siden spr\xE5ket ikke inkluderer en innebygd parser for\
  \ YAML. Et\u2026"
lastmod: '2024-03-13T22:44:41.203617-06:00'
model: gpt-4-0125-preview
summary: "I JavaScript inneb\xE6rer arbeid med YAML vanligvis bruk av et tredjeparts\
  \ bibliotek siden spr\xE5ket ikke inkluderer en innebygd parser for YAML."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
I JavaScript innebærer arbeid med YAML vanligvis bruk av et tredjeparts bibliotek siden språket ikke inkluderer en innebygd parser for YAML. Et av de mest populære bibliotekene til dette formålet er `js-yaml`. Du kan bruke `js-yaml` til å parse YAML til JavaScript-objekter og motsatt.

Først må du installere `js-yaml`:

```bash
npm install js-yaml
```

Deretter kan du bruke det i prosjektene dine. Slik kan du laste en YAML-fil og parse den til et JavaScript-objekt:

```javascript
// Krev js-yaml-modulen
const yaml = require('js-yaml');
const fs   = require('fs');

// Last inn YAML fra en fil
prøv {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Hvis din `config.yaml` fil ser slik ut:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Vil utdataen være:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

For å gjøre det omvendte, å konvertere et JavaScript-objekt til en YAML-streng:

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

Denne koden vil produsere:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Ved å bruke `js-yaml`, kan du enkelt integrere YAML-parsing og serialisering i JavaScript-prosjektene dine, noe som forbedrer datautvekslingsbarheten og konfigurasjonsstyringen.
