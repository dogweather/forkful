---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:42.557424-07:00
description: "Miten: Node.js:ss\xE4 stderriin kirjoittaminen voidaan saavuttaa k\xE4\
  ytt\xE4m\xE4ll\xE4 `console.error()`-metodia tai kirjoittamalla suoraan `process.stderr`iin.\
  \ T\xE4ss\xE4\u2026"
lastmod: '2024-03-13T22:44:56.966241-06:00'
model: gpt-4-0125-preview
summary: "Node.js:ss\xE4 stderriin kirjoittaminen voidaan saavuttaa k\xE4ytt\xE4m\xE4\
  ll\xE4 `console.error()`-metodia tai kirjoittamalla suoraan `process.stderr`iin."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

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
