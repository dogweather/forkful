---
aliases:
- /fi/javascript/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:45.352804-07:00
description: "YAML, lyhenteen\xE4 ilmaisulle YAML Ain't Markup Language, on ihmisen\
  \ luettavaa tietojen sarjallistamismuotoa. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4\
  \ usein\u2026"
lastmod: 2024-02-18 23:09:08.058098
model: gpt-4-0125-preview
summary: "YAML, lyhenteen\xE4 ilmaisulle YAML Ain't Markup Language, on ihmisen luettavaa\
  \ tietojen sarjallistamismuotoa. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 usein\u2026"
title: "Ty\xF6skentely YAML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML, lyhenteenä ilmaisulle YAML Ain't Markup Language, on ihmisen luettavaa tietojen sarjallistamismuotoa. Ohjelmoijat käyttävät sitä usein konfiguraatiotiedostoissa ja tietojen vaihdossa kielten välillä sen yksinkertaisuuden ja luettavuuden vuoksi verrattuna JSONiin tai XML:ään.

## Kuinka:

JavaScriptissä YAMLin kanssa työskentely tapahtuu tyypillisesti käyttämällä kolmannen osapuolen kirjastoa, koska kieli ei sisällä sisäänrakennettua jäsentäjää YAMLille. Yksi suosituimmista kirjastoista tähän tarkoitukseen on `js-yaml`. Voit käyttää `js-yaml`ia jäsentämään YAMLia JavaScript-objekteiksi ja päinvastoin.

Ensin sinun on asennettava `js-yaml`:

```bash
npm install js-yaml
```

Sitten, voit käyttää sitä projekteissasi. Näin voit ladata YAML-tiedoston ja jäsentää sen JavaScript-objektiksi:

```javascript
// Vaadi js-yaml-moduuli
const yaml = require('js-yaml');
const fs   = require('fs');

// Lataa YAML tiedostosta
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Jos `config.yaml`-tiedostosi näyttää tältä:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Tulos on:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Tehdäksesi päinvastaisen, muuntaaksesi JavaScript-objektin YAML-merkkijonoksi:

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

Tämä koodi tuottaa:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Käyttämällä `js-yaml`ia, voit helposti integroida YAMLin jäsentämisen ja sarjallistamisen JavaScript-projekteihisi, parantamaan tiedon vaihtokelpoisuutta ja konfiguraationhallintaa.
