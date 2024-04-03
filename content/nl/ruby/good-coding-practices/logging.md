---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:02.684434-07:00
description: "Loggen in programmeren is als het bijhouden van een dagboek voor je\
  \ applicatie. Het is het systematisch vastleggen van gebeurtenissen, berichten en\u2026"
lastmod: '2024-03-13T22:44:51.345147-06:00'
model: gpt-4-0125-preview
summary: Loggen in programmeren is als het bijhouden van een dagboek voor je applicatie.
title: Logboekregistratie
weight: 17
---

## Wat & Waarom?
Loggen in programmeren is als het bijhouden van een dagboek voor je applicatie. Het is het systematisch vastleggen van gebeurtenissen, berichten en gegevenspunten die je inzicht geven in wat je applicatie doet en hoe deze zich gedraagt. Programmeurs loggen omdat het cruciaal is voor het debuggen, het bewaken van de gezondheid van de applicatie, en het krijgen van aanwijzingen over mogelijke problemen voordat ze uitgroeien tot echte problemen.

## Hoe te:
Ruby komt met een ingebouwde module voor loggen, `Logger`, die super eenvoudig te gebruiken is. Hier is een snel voorbeeld om je op weg te helpen:

```ruby
require 'logger'

# Creëer een Logger die uitvoert naar STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Voorbeeld logberichten
logger.info("Dit is een informatiebericht")
logger.warn("Dit is een waarschuwingsbericht")
logger.error("Dit is een foutbericht")
```

Het uitvoeren van het bovenstaande script zal zoiets als dit opleveren:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Dit is een informatiebericht
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Dit is een waarschuwingsbericht
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Dit is een foutbericht
```

Je kunt het logformaat en -niveau configureren om onnodige ruis te filteren, en je kunt logs naar verschillende uitvoerlocaties sturen, zoals een bestand of zelfs een externe logservice.

## Diepgaande duik
Loggen is als een eeuwenoude traditie in programmeren. Historisch gezien waren logs eenvoudige tekstbestanden, handmatig geparseerd met tools zoals `grep`. Maar het concept groeide uit tot een heel ecosysteem van robuuste logframeworks en -services zoals Log4j, Syslog op Linux, of Sematext en Loggly in het cloudtijdperk.

Ruby's `Logger` is een eenvoudige manier om te beginnen, maar als je meer kracht en flexibiliteit nodig hebt, kun je alternatieven zoals Lograge of Semantic Logger bekijken. Deze bibliotheken werken goed met Ruby applicaties, en bieden meer granulaire controle over logformattering, inclusief gestructureerde logs (JSON-formaat), betere prestaties en naadloze integratie met andere services.

Elke Ruby logbibliotheek heeft zijn eigen manier van werken, maar onder de motorkap draaien ze allemaal om het idee van een loggerinstantie waarnaar je berichten stuurt. De logger behandelt deze berichten op basis van ingestelde niveaus—DEBUG, INFO, WARN, ERROR, FATAL en UNKNOWN—en beslist wat ermee te doen: ze uitprinten, opslaan in een bestand, over het netwerk versturen, enz.

## Zie ook
Voor een diepgaande duik in Ruby's ingebouwde logmodule, bekijk de officiële documentatie:

Als je geïnteresseerd bent in geavanceerder loggen of externe gems wilt verkennen:
- [Lograge](https://github.com/roidrage/lograge)

Voor algemene logpraktijken en filosofie (niet specifiek voor Ruby), zijn deze artikelen tijdloos:
- [Google's Site Reliability Engineering Boek - Hoofdstuk 16: Het omgaan met overbelasting](https://sre.google/sre-book/handling-overload/#log-messages)
- [The 12 Factor App - Logs](https://12factor.net/logs)
