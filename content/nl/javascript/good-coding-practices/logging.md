---
title:                "Logboekregistratie"
aliases:
- /nl/javascript/logging/
date:                  2024-01-28T22:02:59.716826-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in een notendop als het bijhouden van een dagboek voor je applicatie - het registreert gebeurtenissen, fouten en andere belangrijke acties die plaatsvinden terwijl de software draait. Programmeurs doen dit niet alleen om te begrijpen wat er onder de motorkap gebeurt in real-time, maar ook om een historisch register te hebben dat cruciaal is voor debugging, auditing en het optimaliseren van prestaties.

## Hoe te:
Uit de doos biedt JavaScript een eenvoudige manier om berichten naar de console te loggen:

```javascript
console.log('Dit wordt naar de console gelogd');

// Output:
// Dit wordt naar de console gelogd
```

Maar echte wereld apps vereisen meer dan alleen berichten naar de console printen. Bibliotheken zoals Winston of Pino kunnen worden geïntroduceerd om logs effectief te beheren:

```javascript
// Gebruikmakend van Winston voor geavanceerd loggen
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Hallo, dit is een logevenement met Winston');
// Deze log wordt geschreven naar 'combined.log' in JSON formaat
```

Voorbeeld `combined.log` output:

```json
{"message":"Hallo, dit is een logevenement met Winston","level":"info"}
```

## Diepere Duik
Loggen is essentieel sinds de vroege dagen van de computer; systeemoperators zouden logs doorlopen om de systeemprestaties te begrijpen en problemen te diagnosticeren. Fast forward naar moderne ontwikkeling, en we zijn overgegaan van eenvoudige logbestanden naar gestructureerde en doorzoekbare logbeheersystemen.

Alternatieven voor console of bestand gebaseerd loggen in JavaScript omvatten het gebruik van cloud-gebaseerde logdiensten zoals Loggly, Datadog, of ELK Stack (Elasticsearch, Logstash, Kibana) die logs van meerdere bronnen kunnen aggregeren, visualisatietools en geavanceerde analyses bieden.

Bij het implementeren van loggen, overweeg het volgende:
- **Detailniveau**: Inclusief debug, info, waarschuwing, fout, en kritiek.
- **Prestatie**: Overmatig loggen kan de prestaties van de applicatie beïnvloeden.
- **Veiligheid**: Wees voorzichtig met het loggen van gevoelige informatie.
- **Formaat**: Gestructureerde logs (zoals JSON) maken het gemakkelijker om logs te zoeken en te parseren.
- **Bewaarbeleid**: Oude logs moeten worden gearchiveerd of gewist om ruimte te besparen.

Een praktische logstrategie definieert wat te loggen, waar het te loggen en hoe lang het te bewaren, waarbij informatief inzicht wordt afgewogen tegen prestatie- en privacyoverwegingen.

## Zie Ook
Bekijk deze bronnen voor een diepere duik:
- [Winston GitHub Repository](https://github.com/winstonjs/winston): voor diepgaand gebruik en aangepaste transports.
- [Pino - Zeer lage overhead Node.js logger](https://github.com/pinojs/pino): een lichtgewicht logoplossing.
- [MDN Web Docs: Console](https://developer.mozilla.org/nl/docs/Web/API/Console): voor kerninformatie over loggen in de browser.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): een krachtig trio voor logbeheer.
- [12 Factor App Logging](https://12factor.net/logs): beste praktijken in app loggen.
