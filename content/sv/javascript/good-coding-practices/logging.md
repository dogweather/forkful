---
date: 2024-01-26 01:07:11.981497-07:00
description: "Loggning \xE4r i ett n\xF6tskal som att h\xE5lla en dagbok f\xF6r din\
  \ applikation \u2013 den registrerar h\xE4ndelser, fel och andra signifikanta \xE5\
  tg\xE4rder som sker medan\u2026"
lastmod: '2024-03-13T22:44:38.299776-06:00'
model: gpt-4-1106-preview
summary: "Loggning \xE4r i ett n\xF6tskal som att h\xE5lla en dagbok f\xF6r din applikation\
  \ \u2013 den registrerar h\xE4ndelser, fel och andra signifikanta \xE5tg\xE4rder\
  \ som sker medan\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är i ett nötskal som att hålla en dagbok för din applikation – den registrerar händelser, fel och andra signifikanta åtgärder som sker medan programvaran körs. Programmerare gör detta inte bara för att förstå vad som pågår under huven i realtid, men även för att ha en historisk journal som är avgörande för felsökning, revision och optimering av prestanda.

## Hur man gör:
Rät ur lådan erbjuder JavaScript ett enkelt sätt att logga meddelanden till konsolen:

```javascript
console.log('Detta kommer att loggas till konsolen');

// Utmatning:
// Detta kommer att loggas till konsolen
```

Men riktiga applikationer kräver mer än att bara skriva ut meddelanden till konsolen. Bibliotek som Winston eller Pino kan introduceras för att effektivt hantera loggar:

```javascript
// Använda Winston för avancerad loggning
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Hej, detta är en logghändelse med Winston');
// Denna logg skrivs till 'combined.log' i JSON-format
```

Exempel `combined.log` utmatning:

```json
{"message":"Hej, detta är en logghändelse med Winston","level":"info"}
```

## Fördjupning
Loggning har varit nödvändigt ända sedan datorteknikens tidiga dagar; systemoperatörer skulle granska loggar för att förstå systemprestanda och diagnostisera problem. Ser vi framåt till dagens utveckling så har vi gått från enkla loggfiler till strukturerade och sökbara logghanteringssystem.

Alternativ till konsol- eller filbaserad loggning i JavaScript inkluderar att använda molnbaserade loggtjänster som Loggly, Datadog eller ELK Stack (Elasticsearch, Logstash, Kibana) som kan sammanställa loggar från flera källor, erbjuda visualiseringsverktyg och avancerad analys.

När du implementerar loggning, överväg följande:
- **Detaljnivå**: Inkluderar debug, info, varning, fel och kritisk.
- **Prestanda**: Överdriven loggning kan påverka applikationens prestanda.
- **Säkerhet**: Var försiktig med att logga känslig information.
- **Format**: Strukturerade loggar (som JSON) gör det enklare att söka och tolka loggar.
- **Bevarandepolicys**: Gamla loggar behöver arkiveras eller rensas för att spara utrymme.

En praktisk loggningsstrategi definierar vad som ska loggas, var det ska loggas och hur länge det ska sparas och balanserar informativa insikter mot prestanda- och integritetshänsyn.

## Se även
Kolla in dessa resurser för en djupare dykning:
- [Winston GitHub-förvar](https://github.com/winstonjs/winston): för djupgående användning och anpassade transportvägar.
- [Pino - Mycket låg overhead Node.js logger](https://github.com/pinojs/pino): en lättviktig logglösning.
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): för grundläggande webbläsarbaserad loggningsinfo.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): en kraftfull trio för att hantera loggar.
- [12 Factor App Logging](https://12factor.net/logs): bästa praxis inom applikationsloggning.
