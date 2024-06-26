---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:08.614804-07:00
description: 'Hoe te: Om de huidige datum en tijd te halen, gebruik je `date`. Hier
  is de eenvoudige manier.'
lastmod: '2024-03-13T22:44:50.993708-06:00'
model: gpt-4-0125-preview
summary: Om de huidige datum en tijd te halen, gebruik je `date`.
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
Om de huidige datum en tijd te halen, gebruik je `date`. Hier is de eenvoudige manier:

```Bash
date
```

En bam, je krijgt zoiets als dit:

```
Mon Mar 27 12:45:21 PDT 2023
```

Heb je de datum in een ander formaat nodig? Geen probleem. Gebruik de `+%` opties:

```Bash
date +"%Y-%m-%d"
```

De output is nu helemaal netjes en opgeruimd:

```
2023-03-27
```

## Diep Duiken
Vroeger hadden systemen niet altijd interne klokken. Daarom vertrouwden mensen op tijddelingsystemen om de tijd te krijgen. Vandaag de dag weet elk systeem waar je Bash op draait de tijd. Dank `date`.

`date` is veelzijdig. Wil je de datum van volgende week? Voeg gewoon een fancy `--date` vlag toe:

```Bash
date --date="next week"
```

Maar wacht, er is meer! Heb je een andere tijdzone in gedachten?

```Bash
TZ="Europe/Paris" date
```

Nu krijg je de tijd van Parijs. Fancy.

Bash staat niet alleen in het spel van datums halen. Python, PHP, JavaScript – ze hebben allemaal hun eigen manieren. Maar in het rijk van shell scripting is `date` je trouwe sidekick.

Waarom doet dit ertoe? Automatisering, mijn vriend. Scripts die dingen doen afhankelijk van de datum en tijd, vertrouwen op `date`. Cronjobs? Die houden van een goede tijdstempel.

Hier is de technische uitleg: `date` haalt informatie uit de systeemklok, die gesynchroniseerd wordt met hardware of netwerktijdbronnen, dus je leeft niet in het verleden.

## Zie Ook
- Bekijk `man date` voor een opwindende lezing over alles wat met `date` te maken heeft.
- Wip langs Greg's Wiki voor enkele verlegen scriptingtips: http://mywiki.wooledge.org/BashGuide
- Als je op zoek bent naar meer, is er altijd nog de handleiding van GNU coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
