---
aliases:
- /no/ruby/logging/
date: 2024-01-26 01:08:31.288961-07:00
description: "Logging i programmering er som \xE5 holde en dagbok for applikasjonen\
  \ din. Det er den systematiske opptegnelsen av hendelser, meldinger og datapunkter\
  \ som\u2026"
lastmod: 2024-02-18 23:08:54.444541
model: gpt-4-1106-preview
summary: "Logging i programmering er som \xE5 holde en dagbok for applikasjonen din.\
  \ Det er den systematiske opptegnelsen av hendelser, meldinger og datapunkter som\u2026"
title: "Loggf\xF8ring"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging i programmering er som å holde en dagbok for applikasjonen din. Det er den systematiske opptegnelsen av hendelser, meldinger og datapunkter som gir deg innsikt i hva applikasjonen din gjør og hvordan den oppfører seg. Kodeutviklere logger fordi det er avgjørende for feilsøking, overvåking av applikasjonshelse og for å få hint om potensielle problemer før de utvikler seg til virkelige utfordringer.

## Hvordan gjøre det:
Ruby leveres med en innebygd modul for logging, `Logger`, som er superenkel å bruke. Her er et kjapt eksempel for å komme i gang:

```ruby
require 'logger'

# Opprett en Logger som outputter til STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Eksempel på loggmeldinger
logger.info("Dette er en info-melding")
logger.warn("Dette er en advarsel-melding")
logger.error("Dette er en feilmelding")
```

Å kjøre det ovenstående skriptet vil utgi noe som dette:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Dette er en info-melding
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Dette er en advarsel-melding
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Dette er en feilmelding
```

Du kan konfigurere loggformat og -nivå for å filtrere ut unødvendig støy, og du kan dirigere logger til forskjellige utdata, som en fil eller til og med en ekstern loggtjeneste.

## Dypdykk
Logging er som en eldgammel tradisjon i programmering. Historisk sett var logger enkle tekstfiler, manuelt parsede med verktøy som `grep`. Men konseptet utviklet seg til et helt økosystem av robuste loggingrammeverker og -tjenester slik som Log4j, Syslog på Linux, eller Sematext og Loggly i skyløsningenes æra.

Rubys `Logger` er en enkel måte å komme i gang på, men hvis du trenger mer kraft og fleksibilitet, kan du sjekke ut alternativer som Lograge eller Semantic Logger. Disse bibliotekene fungerer godt med Ruby-applikasjoner og tilbyr mer detaljert kontroll over loggformatering, inkludert strukturerte logger (JSON-format), bedre ytelse og sømløs integrasjon med andre tjenester.

Hvert Ruby loggingbibliotek har sin egen måte å gjøre ting på, men under overflaten dreier de seg alle rundt ideen om en loggerinstans som du sender meldinger til. Loggeren håndterer disse meldingene basert på satt nivå—DEBUG, INFO, WARN, ERROR, FATAL og UNKNOWN—og bestemmer hva de skal gjøre med dem: skrive dem ut, lagre dem til en fil, sende dem over nettverket osv.

## Se også
For et dypdykk i Rubys innebygde loggingmodul, sjekk ut den offisielle dokumentasjonen:

Hvis du er interessert i mer avansert logging eller ønsker å utforske tredjeparts gems:
- [Lograge](https://github.com/roidrage/lograge)

For generelle loggingpraksiser og -filosofi (ikke Ruby-spesifikk), er disse artiklene tidløse lesninger:
- [Googles Site Reliability Engineering Book - Kapittel 16: Handling Overload](https://sre.google/sre-book/handling-overload/#log-messages)
- [The 12 Factor App - Logger](https://12factor.net/logs)
