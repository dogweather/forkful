---
title:                "Het huidige datum ophalen"
aliases:
- /nl/bash/getting-the-current-date.md
date:                  2024-01-28T22:01:08.614804-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De huidige datum in bash pakken gaat over het verkrijgen van de datum en tijd van het systeem. Programmeurs hebben dit nodig voor logboekregistratie, tijdstempeling of het plannen van taken.

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
