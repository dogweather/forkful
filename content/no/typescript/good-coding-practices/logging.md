---
date: 2024-01-26 01:08:56.302597-07:00
description: "Logging er prosessen med \xE5 registrere hendelser, feil og annen viktig\
  \ informasjon under kj\xF8ringen av et program til et eksternt medium, ofte filer\
  \ eller\u2026"
lastmod: 2024-02-19 22:04:59.781601
model: gpt-4-1106-preview
summary: "Logging er prosessen med \xE5 registrere hendelser, feil og annen viktig\
  \ informasjon under kj\xF8ringen av et program til et eksternt medium, ofte filer\
  \ eller\u2026"
title: "Loggf\xF8ring"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Logging er prosessen med å registrere hendelser, feil og annen viktig informasjon under kjøringen av et program til et eksternt medium, ofte filer eller databaser. Programmerere bruker logger for å overvåke programvareatferd, feilsøke problemer og spore systemaktiviteter for sikkerhets- og ytelsesanalyse.

## Hvordan:

I TypeScript kan du enkelt implementere grunnleggende logging ved hjelp av konsollmetoder eller integrere mer avansert logging med biblioteker som `winston` eller `pino`. Her er et grunnleggende eksempel som bruker `console.log` og et mer avansert ett med `winston`.

```TypeScript
// Grunnleggende konsoll-logging
console.log('Info: Starter applikasjonen...');
console.error('Feil: Kunne ikke hente data.');

// Eksempel på utskrift
// Info: Starter applikasjonen...
// Feil: Kunne ikke hente data.
```

For mer robust logging, la oss sette opp `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transporter: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Serveren startet!');
logger.warn('Advarsel: Lav diskplass.');
logger.error('Klarte ikke å koble til databasen.');

// Eksempel på utskrift i combined.log
// 2023-01-20 14:42:07 info: Serveren startet!
// 2023-01-20 14:42:09 warn: Advarsel: Lav diskplass.
// 2023-01-20 14:42:12 error: Klarte ikke å koble til databasen.
```

## Dypdykk:

Konseptet logging innen databehandling går tilbake til de tidlige dagene av programmering, hvor selve uttrykket er avledet fra "logg", et nautisk system for å føre oppføringer. Historisk sett ble programhendelser ofte logget til fysiske utskrifter eller terminalutganger, spesielt under stormaskinæraen.

Hvis vi spoler frem til i dag, har du et vell av verktøy og biblioteker til disposisjon som imøtekommer ulike loggebehov, fra enkle tekstfiler til komplekse loggestyringssystemer. Alternativer til `winston` inkluderer `pino`, som skryter av høy ytelse, og `Bunyan`, som er basert på JSON. Når du jobber med Node.js, tilbyr loggingbiblioteker ofte strømmemekanismer for å kanalisere logger til forskjellige bestemmelsessteder, støtte for loggrotasjon og tilpassbare formaterere.

Når det gjelder implementering, inneholder loggmeldinger vanligvis et tidsstempel, et alvorlighetsnivå (som info, warn, error) og selve meldingen. God loggepraksis anbefaler å kategorisere loggnivåene skikkelig, unngå sensitiv data i logger og vurdere ytelsesimplikasjoner i applikasjoner med høy gjennomstrømning.

## Se Også:

- [Winston - En logger for så og si alt](https://www.npmjs.com/package/winston)
- [Pino - Veldig rask Node.js logger](https://www.npmjs.com/package/pino)
- [Beste praksiser for logging i Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [Den 12-faktor appen - Logger](https://12factor.net/logs)
