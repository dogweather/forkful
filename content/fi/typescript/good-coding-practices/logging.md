---
date: 2024-01-26 01:09:21.008477-07:00
description: "Kuinka: TypeScriptiss\xE4 voit toteuttaa peruslokauksen helposti k\xE4\
  ytt\xE4en konsolin metodeja tai integroida kehittyneemm\xE4n lokauksen kirjastojen,\
  \ kuten\u2026"
lastmod: '2024-03-13T22:44:56.321650-06:00'
model: gpt-4-1106-preview
summary: "TypeScriptiss\xE4 voit toteuttaa peruslokauksen helposti k\xE4ytt\xE4en\
  \ konsolin metodeja tai integroida kehittyneemm\xE4n lokauksen kirjastojen, kuten\
  \ `winston` tai `pino`, avulla."
title: Lokitus
weight: 17
---

## Kuinka:
TypeScriptissä voit toteuttaa peruslokauksen helposti käyttäen konsolin metodeja tai integroida kehittyneemmän lokauksen kirjastojen, kuten `winston` tai `pino`, avulla. Tässä on perusesimerkki `console.log`-toiminnon käytöstä ja kehittyneempi esimerkki `winston`-kirjaston avulla.

```TypeScript
// Perus konsolilokitus
console.log('Info: Sovelluksen käynnistäminen...');
console.error('Virhe: Tietojen noutaminen epäonnistui.');

// Esimerkkivastaus
// Info: Sovelluksen käynnistäminen...
// Virhe: Tietojen noutaminen epäonnistui.
```

Rakentavampaa lokitusta varten otetaan käyttöön `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Palvelin käynnistetty!');
logger.warn('Varoitus: Levytilaa vähän jäljellä.');
logger.error('Tietokantaan yhdistäminen epäonnistui.');

// Esimerkkisisältö tiedostossa combined.log
// 2023-01-20 14:42:07 info: Palvelin käynnistetty!
// 2023-01-20 14:42:09 warn: Varoitus: Levytilaa vähän jäljellä.
// 2023-01-20 14:42:12 error: Tietokantaan yhdistäminen epäonnistui.
```

## Syväsukellus:
Lokituksen käsite tietotekniikan kontekstissa juontaa juurensa ohjelmoinnin varhaisiin päiviin, jossa termi itse on peräisin "lokikirjasta", merenkulun kirjanpidon järjestelmästä. Historiallisesti ohjelmatapahtumat kirjattiin usein fyysisiin tulosteisiin tai terminaalin tulosteisiin, erityisesti pääteyhteysaikakaudella.

Päivämme tietotekniikassa on käytettävissä runsaasti työkaluja ja kirjastoja, jotka palvelevat erilaisia lokitustarpeita, yksinkertaisista tekstiedostoista monimutkaisiin lokituksen hallintajärjestelmiin. Vaihtoehtoja `winston`-kirjastolle ovat muun muassa `pino`, joka on tunnettu suorituskyvystään, ja `Bunyan`, joka perustuu JSON-muotoon. Työskennellessä Node.js:n kanssa lokituskirjastot tarjoavat usein virtamekanismeja ohjaamaan lokia eri kohteisiin, tukea lokitiedostojen kierrätykselle ja mukautettavia muotoilijoita.

Toteutuksen kannalta lokiviestit sisältävät yleensä aikaleiman, vakavuustason (kuten info, warning, error) sekä itse viestin. Hyvä lokikäytäntö suosittelee lokitasojen asianmukaista kategorisointia, arkaluontoisen tiedon välttämistä lokeissa sekä suorituskyvyn harkintaa korkea-liikenteisissä sovelluksissa.

## Katso myös:
- [Winston - Loki melkein kaikkeen](https://www.npmjs.com/package/winston)
- [Pino - Erittäin pieni ylikuorma Node.js lokittaja](https://www.npmjs.com/package/pino)
- [Node.js lokitus parhaat käytännöt](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [12 Factor App - Lokit](https://12factor.net/logs)
