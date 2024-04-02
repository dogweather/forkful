---
date: 2024-01-26 01:07:14.755801-07:00
description: "Logging, kort fortalt, er som \xE5 f\xF8re en dagbok for din applikasjon\
  \ \u2013 det registrerer hendelser, feil og andre vesentlige handlinger som finner\
  \ sted mens\u2026"
lastmod: '2024-03-13T22:44:41.189914-06:00'
model: gpt-4-1106-preview
summary: "Logging, kort fortalt, er som \xE5 f\xF8re en dagbok for din applikasjon\
  \ \u2013 det registrerer hendelser, feil og andre vesentlige handlinger som finner\
  \ sted mens\u2026"
title: "Loggf\xF8ring"
weight: 17
---

## Hva & Hvorfor?
Logging, kort fortalt, er som å føre en dagbok for din applikasjon – det registrerer hendelser, feil og andre vesentlige handlinger som finner sted mens programvaren kjører. Programmerere gjør dette ikke bare for å forstå hva som skjer under panseret i sanntid, men også for å ha en historisk oversikt som er avgjørende for feilsøking, revisjon og optimalisering av ytelse.

## Hvordan gjøre det:
Rett ut av boksen, tilbyr JavaScript en enkel måte å logge meldinger til konsollen på:

```javascript
console.log('Dette vil bli logget til konsollen');

// Utdata:
// Dette vil bli logget til konsollen
```

Men virkelige applikasjoner krever mer enn bare å skrive ut meldinger til konsollen. Biblioteker som Winston eller Pino kan introduseres for å effektivt håndtere logger:

```javascript
// Bruker Winston for avansert logging
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Hallo, dette er en logghendelse med Winston');
// Denne loggen er skrevet til 'combined.log' i JSON-format
```

Eksempel på `combined.log` utdata:

```json
{"message":"Hallo, dette er en logghendelse med Winston","level":"info"}
```

## Dypdykk
Logging har vært essensiell siden datamaskinenes barndom; systemoperatører ville gjennomgå logger for å forstå systemets ytelse og diagnostisere problemer. Når vi spoler frem til moderne utvikling, har vi gått fra enkle loggfiler til strukturerte og søkbare loggadministrasjonssystemer.

Alternativer til konsoll- eller filbasert logging i JavaScript inkluderer å bruke skybaserte loggingtjenester som Loggly, Datadog eller ELK Stack (Elasticsearch, Logstash, Kibana) som kan samle logger fra flere kilder, tilby visualiseringsverktøy og avansert analytikk.

Når du implementerer logging, vurder følgende:
- **Detaljnivå**: Inkluderer debug, info, advarsel, feil og kritisk.
- **Ytelse**: Overdreven logging kan påvirke applikasjonens ytelse.
- **Sikkerhet**: Vær forsiktig med logging av sensitiv informasjon.
- **Format**: Strukturerte logger (som JSON) gjør det enklere å søke og tolke logger.
- **Retningslinjer for Oppbevaring**: Gamle logger må arkiveres eller slettes for å spare plass.

En praktisk strategi for logging definerer hva som skal logges, hvor det skal logges og hvor lenge det skal oppbevares, og balanserer informativ innsikt mot hensyn til ytelse og personvern.

## Se også
Sjekk ut disse ressursene for et dypere dypdykk:
- [Winston GitHub-repositorium](https://github.com/winstonjs/winston): for detaljert bruk og egendefinerte transportører.
- [Pino - Meget lav overhead Node.js-logger](https://github.com/pinojs/pino): en lettvekts løsning for logging.
- [MDN Web Docs: Konsoll](https://developer.mozilla.org/en-US/docs/Web/API/Console): for grunnleggende informasjon om logging basert i nettleser.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): en kraftig trio for logghåndtering.
- [12 Factor App Logging](https://12factor.net/logs): beste praksis for applikasjonslogging.
