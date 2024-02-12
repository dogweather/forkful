---
title:                "Kirjoittaminen standardivirheeseen"
aliases: - /fi/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:34:46.924887-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
TypeScriptissä standardivirheeseen (stderr) kirjoittaminen tarkoittaa virheviestien tai lokien suoraan lähettämistä ympäristön virhetulostevirtaan (esim. node.js:n konsoli tai web-selain). Tämä on olennaista ongelmien diagnosoinnissa häiritsemättä standarditulostetta (stdout), jota käytetään tyypillisesti ohjelman datan esittämiseen, varmistaen, että virheenkäsittely ja lokitus hoidetaan tehokkaasti ja yhtenäisesti.

## Kuinka:
TypeScript, ollessaan JavaScriptin yläjoukko, nojautuu alla olevaan JS-ajoaikaympäristöön (kuten Node.js) kirjoittaessaan stderr:iin. Tässä on miten voit tehdä sen suoraan:

```typescript
console.error("Tämä on virheviesti.");
```

Esimerkkitulostus stderriin:
```
Tämä on virheviesti.
```

Node.js-ympäristössä voit myös käyttää `process.stderr.write()` -metodia matalamman tason kirjoittamiseen:

```typescript
process.stderr.write("Matalan tason virheviesti.\n");
```

Esimerkkitulostus stderriin:
```
Matalan tason virheviesti.
```

Rakenteellisempaan virhelokitus käyttöön saatat haluta käyttää suosittuja kolmannen osapuolen kirjastoja kuten `winston` tai `pino`. Tässä on miten kirjaat virheitä käyttäen `winstonia`:

Ensiksi, asenna `winston`:

```bash
npm install winston
```

Sitten käytä sitä TypeScript-tiedostossasi:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Winstonilla kirjattu virhe.');
```

Tämä kirjoittaa virheen sekä konsoliin että tiedostoon nimeltä `error.log`. Muista, että tiedostoihin kirjoitettaessa on tärkeää hallita tiedostojen oikeuksia ja rolloveria välttääksesi ongelmat, jotka liittyvät levytilan käyttöön.
