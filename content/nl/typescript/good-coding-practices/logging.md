---
title:                "Logboekregistratie"
aliases:
- /nl/typescript/logging.md
date:                  2024-01-28T22:03:09.458964-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen is het proces van het vastleggen van gebeurtenissen, fouten en andere significante informatie tijdens de uitvoering van een programma naar een externe drager, vaak bestanden of databases. Programmeurs gebruiken logboeken om softwaregedrag te monitoren, problemen te debuggen en systeemactiviteiten te volgen voor veiligheids- en prestatieanalyse.

## Hoe te:

In TypeScript kun je gemakkelijk basislogboekregistratie implementeren met consolemethoden of meer geavanceerde logboekregistratie integreren met bibliotheken zoals `winston` of `pino`. Hier is een basisvoorbeeld met `console.log` en een geavanceerder voorbeeld met `winston`.

```TypeScript
// Basis console logboekregistratie
console.log('Info: Starten van de applicatie...');
console.error('Fout: Kan gegevens niet ophalen.');

// Voorbeelduitvoer
// Info: Starten van de applicatie...
// Fout: Kan gegevens niet ophalen.
```

Voor robuustere logboekregistratie, laten we `winston` instellen:

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

logger.info('Server gestart!');
logger.warn('Waarschuwing: weinig schijfruimte.');
logger.error('Kan geen verbinding maken met database.');

// Voorbeelduitvoer in combined.log
// 2023-01-20 14:42:07 info: Server gestart!
// 2023-01-20 14:42:09 waarschuwing: Weinig schijfruimte.
// 2023-01-20 14:42:12 fout: Kan geen verbinding maken met database.
```

## Diepgaand:

Het concept van logboekregistratie binnen de context van computergebruik gaat terug tot de vroege dagen van programmeren, waar de term zelf is afgeleid van het "logboek", een maritiem recordhoudsysteem. Historisch gezien werden programmagebeurtenissen vaak gelogd naar fysieke afdrukken of terminaluitvoer, vooral tijdens het mainframe-tijdperk.

Fast forward naar vandaag, en je hebt een overvloed aan tools en bibliotheken tot je beschikking die voldoen aan verschillende logboekregistratiebehoeften, van eenvoudige tekstbestanden tot complexe logboekbeheersystemen. Alternatieven voor `winston` zijn onder andere `pino`, dat een hoge prestatie belooft, en `Bunyan`, dat is gebaseerd op JSON. Wanneer je werkt met Node.js, bieden logboekbibliotheken vaak streammechanismen om logboeken naar verschillende bestemmingen te leiden, ondersteuning voor logrotatie, en aanpasbare formatters.

Wat betreft implementatie bevatten logboodschappen typisch een tijdstempel, een ernstniveau (zoals info, waarschuwing, fout) en de eigenlijke boodschap. Goede logboekpraktijken raden aan om logniveaus correct te categoriseren, gevoelige gegevens in logboeken te vermijden en rekening te houden met prestatie-implicaties in toepassingen met hoge doorvoer.

## Zie Ook:

- [Winston - Een logger voor vrijwel alles](https://www.npmjs.com/package/winston)
- [Pino - Zeer lage overhead Node.js logger](https://www.npmjs.com/package/pino)
- [Beste praktijken voor Node.js logboekregistratie](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [The 12 Factor App - Logboeken](https://12factor.net/logs)
