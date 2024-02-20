---
date: 2024-01-26 01:08:24.953033-07:00
description: "Loggf\xF8ring er prosessen med \xE5 registrere hendelser i en applikasjon\
  \ mens et program kj\xF8rer, og gir en sporlogg for analyse etter hendelsen og overv\xE5\
  kning\u2026"
lastmod: 2024-02-19 22:04:59.649147
model: gpt-4-1106-preview
summary: "Loggf\xF8ring er prosessen med \xE5 registrere hendelser i en applikasjon\
  \ mens et program kj\xF8rer, og gir en sporlogg for analyse etter hendelsen og overv\xE5\
  kning\u2026"
title: "Loggf\xF8ring"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Loggføring er prosessen med å registrere hendelser i en applikasjon mens et program kjører, og gir en sporlogg for analyse etter hendelsen og overvåkning i sanntid. Utviklere gjør dette fordi det hjelper med feilsøking, overvåke ytelse, og spore brukerhandlinger for sikkerhet og analyse.

## Hvordan:
Python kommer med en innebygd modul for loggføring. Her er en enkel oppsett:
```Python
import logging

# Grunnleggende konfigurasjon av loggføringen
logging.basicConfig(level=logging.INFO)

# Loggføringsmeldinger
logging.debug('Dette er en debug-melding')
logging.info('Informasjon om hva programmet ditt nettopp gjorde')
logging.warning('En advarselmelding')
logging.error('Det har oppstått en feil')
logging.critical('Programmet klarer ikke å gjenopprette!')
```
Når du kjører denne koden, vil du se følgende utskrift (siden standardnivået er WARNING, vil ikke debug- og info-meldinger bli vist):
```
WARNING:root:En advarselmelding
ERROR:root:Det har oppstått en feil
CRITICAL:root:Programmet klarer ikke å gjenopprette!
```
Du kan også sette opp loggføring for å skrive til en fil i stedet for konsollen:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Nå vil loggene dine bli dirigert til 'app.log'-filen.

## Dypdykk
Loggføring har vært rundt siden programmeringens tidlige dager, med systemlogger som en av de eldste formene for vedvarende lagring utenfor faktiske filer som holder data. Bortsett fra historien, forblir det grunnleggende konseptet med loggføring stort sett uendret, selv om verktøyene har utviklet seg.

Pythons `logging`-modul er ganske kraftfull og fleksibel. Den lar programmerere sette forskjellige loggnivåer (DEBUG, INFO, WARNING, ERROR, CRITICAL) som kan hjelpe i kategorisering og filtrering av logger. Den har et hierarkisk logger-system, noe som betyr at du kan ha foreldre-barn-relasjoner mellom loggerne og formidle meldinger oppover i kjeden.

Alternativer inkluderer tredjeparts biblioteker som Loguru eller structlog som tilbyr forbedrede funksjoner og et enklere grensesnitt enn den innebygde loggmodulen. De kan tilby penere utskrift, bedre serialisering av strukturerte data, og mer intuitive måter å håndtere loggkonfigurasjon på.

Når det gjelder implementering, er det viktig å sette opp loggføring en gang ved starten av applikasjonen din. Det anbefales å konfigurere det på modulnivå ved å bruke `logging.getLogger(__name__)` for å følge beste praksis for Pythons loggføring.

Loggføring burde ikke påvirke ytelsen til en applikasjon drastisk under normale omstendigheter. Likevel, bør man være forsiktig med hva som logges: altfor detaljert loggføring, spesielt på DEBUG-nivåer, kan bremse ned en applikasjon og raskt fylle opp lagringsplassen for loggfiler.

## Se Også
For mer om Pythons loggmodul, sjekk ut den offisielle Python loggkokboken for noen flotte eksempler og beste praksis: https://docs.python.org/3/howto/logging-cookbook.html

For en grundig titt på strukturert loggføring og hvordan det kan bidra til å gjøre logger mer informative og enklere å analysere, er Loguru godt dokumentert: https://loguru.readthedocs.io

Vurder også å ta en titt på metodologien for 12-faktorapplikasjonen, spesielt avsnittet om logger for det moderne synet på applikasjonsloggføring: https://12factor.net/logs
