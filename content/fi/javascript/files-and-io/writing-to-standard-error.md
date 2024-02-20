---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:42.557424-07:00
description: "Standardivirheeseen (stderr) kirjoittaminen JavaScriptill\xE4 tarkoittaa\
  \ virheilmoitusten tai mink\xE4 tahansa kriittisen tiedon ohjaamista tiettyyn, erilliseen\u2026"
lastmod: 2024-02-19 22:05:15.861054
model: gpt-4-0125-preview
summary: "Standardivirheeseen (stderr) kirjoittaminen JavaScriptill\xE4 tarkoittaa\
  \ virheilmoitusten tai mink\xE4 tahansa kriittisen tiedon ohjaamista tiettyyn, erilliseen\u2026"
title: Kirjoittaminen standardivirheeseen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Standardivirheeseen (stderr) kirjoittaminen JavaScriptillä tarkoittaa virheilmoitusten tai minkä tahansa kriittisen tiedon ohjaamista tiettyyn, erilliseen virtaan, mikä on erityisen hyödyllistä Unix-tyyppisissä ympäristöissä lokiin kirjaamisen ja virheenkorjauksen kannalta. Ohjelmoijat tekevät näin erottaakseen normaalin ohjelman tulosteen virheilmoituksista, mahdollistaen selkeämmän tulostuksen hallinnan ja helpomman virheiden seurannan.

## Miten:
Node.js:ssä stderriin kirjoittaminen voidaan saavuttaa käyttämällä `console.error()`-metodia tai kirjoittamalla suoraan `process.stderr`iin. Tässä esimerkit molemmista lähestymistavoista:

```javascript
// Käyttäen console.error()
console.error('Tämä on virheviesti.');

// Kirjoittaen suoraan process.stderriin
process.stderr.write('Tämä on toinen virheviesti.\n');
```

Molemman menetelmän tuloste näkyy stderr-virrassa, sekoittumatta stdoutiin:
```
Tämä on virheviesti.
Tämä on toinen virheviesti.
```

Monimutkaisempiin tai sovelluskohtaisiin lokitustarpeisiin monet JavaScript-ohjelmoijat käyttävät kolmansien osapuolten kirjastoja, kuten `winston` tai `bunyan`. Tässä pikainen esimerkki käyttäen `winston`:
 
Ensin asenna `winston` npm:n kautta:
```shell
npm install winston
```

Sitten konfiguroi `winston` kirjaamaan virheet stderriin:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Kirjaten virheviestin
logger.error('Virhe kirjattu winstonin kautta.');
```

Tämä asetus varmistaa, että kun loggaat virheen käyttäen `winston`ia, se ohjautuu stderriin, auttaen ylläpitämään selvää eroa normaalin ja virhetulosteen välillä.
