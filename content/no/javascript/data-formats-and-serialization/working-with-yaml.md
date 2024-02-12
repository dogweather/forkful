---
title:                "Arbeider med YAML"
aliases: - /no/javascript/working-with-yaml.md
date:                  2024-02-03T19:25:36.601474-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML, forkortelse for YAML Ain't Markup Language, er et menneskelesbart data serialiseringsformat. Programmerere bruker det ofte for konfigurasjonsfiler og datautveksling mellom språk på grunn av dets enkelhet og lesbarhet sammenlignet med JSON eller XML.

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
