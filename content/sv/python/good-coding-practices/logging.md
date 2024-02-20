---
date: 2024-01-26 01:08:30.522160-07:00
description: "Loggning \xE4r processen att spela in h\xE4ndelser i en applikation\
  \ medan ett program k\xF6rs, vilket ger ett sp\xE5r av \"smulor\" f\xF6r analys\
  \ efter h\xE4ndelsen och f\xF6r\u2026"
lastmod: 2024-02-19 22:04:56.730804
model: gpt-4-1106-preview
summary: "Loggning \xE4r processen att spela in h\xE4ndelser i en applikation medan\
  \ ett program k\xF6rs, vilket ger ett sp\xE5r av \"smulor\" f\xF6r analys efter\
  \ h\xE4ndelsen och f\xF6r\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är processen att spela in händelser i en applikation medan ett program körs, vilket ger ett spår av "smulor" för analys efter händelsen och för realtidsövervakning. Programmerare gör detta eftersom det hjälper till med att felsöka problem, övervaka prestanda och spåra användaråtgärder för säkerhet och analys.

## Hur man gör:
Python kommer med en inbyggd modul för loggning. Här är en grundläggande uppsättning:
```Python
import logging

# Grundläggande konfiguration av loggningen
logging.basicConfig(level=logging.INFO)

# Loggmeddelanden
logging.debug('Detta är ett debug-meddelande')
logging.info('Information om vad ditt program just gjorde')
logging.warning('Ett varningsmeddelande')
logging.error('Ett fel har inträffat')
logging.critical('Programmet kan inte återhämta sig!')
```
När du kör denna kod kommer du att se följande utmatning (eftersom standardnivån är WARNING, kommer debug- och info-meddelanden inte att visas):
```
WARNING:root:Ett varningsmeddelande
ERROR:root:Ett fel har inträffat
CRITICAL:root:Programmet kan inte återhämta sig!
```
Du kan också ställa in loggning för att skriva till en fil istället för till konsolen:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Nu kommer dina loggar att riktas till 'app.log'-filen.

## Fördjupning
Loggning har funnits sedan de tidiga dagarna av programmering, med systemloggar som en av de äldsta formerna av bestående lagring utanför faktiska filer som håller data. Historia åt sidan, det huvudsakliga konceptet av loggning förblir i princip oförändrat, även om verktygen har utvecklats.

Pythons `logging`-modul är mycket kraftfull och flexibel. Den låter programmerare ställa in olika loggnivåer (DEBUG, INFO, WARNING, ERROR, CRITICAL) som kan hjälpa till med att kategorisera och filtrera loggar. Den har ett hierarkiskt loggersystem, vilket innebär att du kan ha förälder-barn-relationer mellan loggers och förmedla meddelanden uppåt i kedjan.

Alternativ inkluderar tredjepartsbibliotek som Loguru eller structlog som erbjuder förbättrade funktioner och ett enklare gränssnitt än den inbyggda loggmodulen. De kan tillhandahålla snyggare utmatning, bättre serialisering av strukturerade data och mer intuitiva sätt att hantera loggkonfiguration.

När det gäller implementering, när du sätter upp loggning är det viktigt att göra det en gång i början av din applikation. Konfigurering på modulnivå rekommenderas med användning av `logging.getLogger(__name__)` för att följa Python loggningsbästa praxis.

Loggning bör inte drastiskt påverka prestandan hos en applikation under normala omständigheter. Dock bör man vara försiktig med vad som loggas: alltför pratig loggning, speciellt på DEBUG-nivåer, kan sakta ner en applikation och snabbt fylla upp lagringsutrymmet för loggfiler.

## Se också
För mer om Pythons loggmodul, kolla in den officiella Python-loggkoken för några fantastiska exempel och bästa praxis: https://docs.python.org/3/howto/logging-cookbook.html

För en djupgående titt på strukturerad loggning och hur det kan hjälpa till att göra loggar mer informativa och lättare att analysera, är Loguru väl dokumenterat: https://loguru.readthedocs.io

Överväg också att titta på 12-faktorappmetodiken, speciellt avsnittet om loggar för den moderna synen på applikationsloggning: https://12factor.net/logs
