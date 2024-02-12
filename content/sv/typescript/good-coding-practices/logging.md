---
title:                "Loggning"
aliases:
- /sv/typescript/logging.md
date:                  2024-01-26T01:09:28.099358-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Loggning är processen att registrera händelser, fel och annan viktig information under ett programs utförande till ett externt medium, ofta filer eller databaser. Programmerare använder loggar för att övervaka mjukvarubeteende, felsöka problem och spåra systemaktivitet för säkerhets- och prestandaanalys.

## Hur man gör:

I TypeScript kan du enkelt implementera grundläggande loggning med hjälp av konsolmetoder eller integrera mer avancerad loggning med bibliotek som `winston` eller `pino`. Här är ett grundläggande exempel som använder `console.log` och ett mer avancerat med `winston`.

```TypeScript
// Grundläggande konsolloggning
console.log('Info: Startar applikationen...');
console.error('Fel: Kunde inte hämta data.');

// Exempel på utskrift
// Info: Startar applikationen...
// Fel: Kunde inte hämta data.
```

För mer robust loggning, låt oss sätta upp `winston`:

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

logger.info('Servern startad!');
logger.warn('Varning för lågt diskutrymme.');
logger.error('Misslyckades att ansluta till databasen.');

// Exempel på utskrift i combined.log
// 2023-01-20 14:42:07 info: Servern startad!
// 2023-01-20 14:42:09 warn: Varning för lågt diskutrymme.
// 2023-01-20 14:42:12 error: Misslyckades att ansluta till databasen.
```

## Djupdykning:

Konceptet loggning inom datavetenskap går tillbaka till programmeringens tidiga dagar, där själva termen härstammar från "loggboken", ett nautiskt system för att föra anteckningar. Historiskt sett loggades programhändelser ofta till fysiska utskrifter eller terminalutdata, särskilt under mainframe-eran.

Fram till idag har du tillgång till ett överflöd av verktyg och bibliotek som tillgodoser olika loggningsbehov, från enkla textfiler till komplexa logghanteringssystem. Alternativ till `winston` inkluderar `pino`, som skryter med hög prestanda, och `Bunyan`, som är baserat på JSON. När man arbetar med Node.js tillhandahåller loggningsbibliotek ofta strömmekanismer för att leda loggar till olika destinationer, stöd för loggrotation och anpassningsbara formaterare.

När det gäller implementeringen innehåller loggmeddelanden vanligtvis en tidsstämpel, en allvarlighetsgrad (såsom info, warn, error) och själva meddelandet. God loggningssed rekommenderar att korrekt kategorisera loggnivåer, undvika känslig data i loggar och beakta prestandaimplikationer i applikationer med hög genomströmning.

## Se även:

- [Winston - En loggare för nästan allt](https://www.npmjs.com/package/winston)
- [Pino - En Node.js-loggare med mycket låg overhead](https://www.npmjs.com/package/pino)
- [Node.js Logging Best Practices](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [The 12 Factor App - Logs](https://12factor.net/logs)
