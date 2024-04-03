---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:46.618527-07:00
description: "Een datum uit een string halen betekent het extraheren van de datumcomponenten\u2014\
  zoals dag, maand en jaar\u2014uit tekst. Programmeurs doen dit om datums in\u2026"
lastmod: '2024-03-13T22:44:50.992695-06:00'
model: gpt-4-0125-preview
summary: "Een datum uit een string halen betekent het extraheren van de datumcomponenten\u2014\
  zoals dag, maand en jaar\u2014uit tekst."
title: Een datum uit een string parsen
weight: 30
---

## Wat & Waarom?

Een datum uit een string halen betekent het extraheren van de datumcomponenten—zoals dag, maand en jaar—uit tekst. Programmeurs doen dit om datums in verschillende formaten te manipuleren of weer te geven, of om tijdverschillen in scripts te berekenen.

## Hoe te:

Gebruik maken van `date` met `+%Y-%m-%d` geeft ons een geformatteerde uitvoer:

```Bash
date_str="Jan 01 2023"
formatted_date=$(date -d "$date_str" '+%Y-%m-%d')
echo $formatted_date
```
```
2023-01-01
```

`date -d` laat ons onze string ontleden, terwijl `+%Y-%m-%d` het uitvoerformaat specificeert.

## Diepere Duik

Bash zelf is niet geweldig in het ontleden van datums. Historisch gezien bevatten Unix-systemen hier geen ingebouwde functie voor. De meeste scripts vertrouwden op externe tools of ingewikkelde omwegen. GNU `date` veranderde het spel met zijn `-d` optie, waardoor eenvoudige datumontleding en uitvoerformatting mogelijk werd.

Alternatieven? Zeker, er zijn `awk`, `sed` en `perl`. Elk heeft zijn eigen manier om het probleem aan te pakken, maar `date` is meestal de eerste keuze vanwege de eenvoud.

De implementatiedetails worden pittiger. `date` gebruikt standaard de systeemlocale-instellingen, wat invloed heeft op hoe het input interpreteert. Het overschrijven van locale kan nodig zijn voor consistent gedrag in verschillende omgevingen. Plus, omgaan met datums vóór 1970 of na 2038? Daar kunnen dingen buggy worden vanwege beperkingen van de Unix-timestamp.

## Zie Ook

- GNU `date` man-pagina: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Meer over Unix-timestamp en het Y2038-probleem: https://nl.wikipedia.org/wiki/2038-probleem
- Datum ontleden in `awk`: https://www.gnu.org/software/gawk/manual/html_node/Time-Functions.html
