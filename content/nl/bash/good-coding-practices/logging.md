---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:39.138761-07:00
description: "Loggen is de praktijk van het vastleggen van gebeurtenissen, fouten\
  \ en andere belangrijke informatie van de draaiende processen van een programma\
  \ naar een\u2026"
lastmod: '2024-03-13T22:44:50.989503-06:00'
model: gpt-4-0125-preview
summary: Loggen is de praktijk van het vastleggen van gebeurtenissen, fouten en andere
  belangrijke informatie van de draaiende processen van een programma naar een bestand
  of een uitvoerstroom.
title: Logboekregistratie
weight: 17
---

## Hoe te:
In Bash kan loggen zo eenvoudig zijn als het omleiden of toevoegen van uitvoer naar een bestand. Hier is een basisvoorbeeld:

```Bash
echo "Het script start..." >> script.log
# Hier komen je scriptopdrachten
echo "Script voltooid op $(date)" >> script.log
```

Voor iets geavanceerders zou je syslog kunnen gebruiken voor systeembrede logging:

```Bash
logger "Aangepaste bericht van mijn script"
```

`logger` stuurt een logbericht naar de syslog-dienst, die het vervolgens afhandelt volgens de syslog-configuratie van het systeem.

Voorbeelduitvoer vastgelegd in `script.log`:

```Bash
Het script start...
Script voltooid op di 23 mrt 09:26:35 PDT 2021
```

## Diepgaand
Historisch gezien, in Unix-achtige systemen, is loggen gefaciliteerd door de syslog-dienst, waardoor verschillende applicaties en delen van het systeem berichten centraal kunnen loggen. Dit biedt de implementatie van een gestandaardiseerd loggingmechanisme door het hele systeem.

Wat betreft alternatieven, sommigen kijken misschien naar `syslog-ng` of `rsyslog` voor geavanceerdere loggingfuncties, of het schrijven van logs naar een tijdreeksdatabase voor analytische doeleinden. Voor applicaties met hogere niveaus van complexiteit, kan het gebruik van een speciale loggingbibliotheek of applicatie zoals Log4j (in het Java-ecosysteem) of Monolog (in PHP), die gestructureerde en configureerbare loggingopties kunnen bieden, zinvol zijn, zelfs voor een scripttaal zoals Bash.

De manier waarop je loggen implementeert, hangt sterk af van de vereisten van je applicatie. Als je gewoon eenvoudige uitvoer nodig hebt om de voortgang van het script bij te houden, is berichten toevoegen aan een bestand gemakkelijk en handig. Echter, voor meer schaalbare en robuuste logboekregistratie, wil je integreren met een logging-systeem dat functies ondersteunt zoals logrotatie, logniveaus en remote logging.

## Zie Ook
- De `man` pagina's voor de `logger` en `syslog` functies zijn altijd je vriend, probeer `man logger` of `man syslog`.
- Voor een diepgaande blik op systeemlogging, overweeg de documentatie van `rsyslog` en `syslog-ng` te lezen.
- Om meer te weten te komen over de historische context en principes achter loggen in Unix-achtige systemen, biedt het `Syslog` protocol gedocumenteerd in RFC 5424 uitgebreide informatie.
