---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:55.575805-07:00
description: "De lengte van een reeks vinden betekent het tellen van het aantal karakters\
  \ erin. Programmeurs doen dit om invoer te valideren, buffers te dimensioneren\u2026"
lastmod: '2024-03-13T22:44:51.236129-06:00'
model: gpt-4-0125-preview
summary: De lengte van een reeks vinden betekent het tellen van het aantal karakters
  erin.
title: De lengte van een string vinden
weight: 7
---

## Wat & Waarom?
De lengte van een reeks vinden betekent het tellen van het aantal karakters erin. Programmeurs doen dit om invoer te valideren, buffers te dimensioneren of door karakters te loopen.

## Hoe te:
Hier is hoe je de lengte van een reeks krijgt in Fish:

```Fish Shell
set my_string "Hallo, Wereld!"
echo (string length "$my_string")
```

Uitvoer:

```
13
```

## Diepgaande duik
In Fish, in tegenstelling tot sommige andere shells, is `string length` een ingebouwde functie waardoor het native en efficiënt is. Historisch gezien hadden andere shells misschien een omslachtigere syntaxis of externe tools zoals `expr` of `wc` nodig. Fish vereenvoudigt taken met zijn robuuste functies voor reeksverwerking, waarbij `string length` direct het aantal Unicode karakters geeft, wat niet altijd gelijk is aan het aantal bytes, vooral voor niet-ASCII karakters.

Alternatieven voor het bepalen van de lengte van een reeks in shells voordat de `string` functie in Fish bestond, konden minder betrouwbaar zijn omdat ze niet altijd rekening hielden met meerdere bytes karakters. Wat de implementatie betreft, telt `string length` Unicode grafemen, wat belangrijk is voor teksten die karakters bevatten die combineren met anderen om een enkele visuele eenheid te vormen.

## Zie ook
- Fish-documentatie over reeksmanipulatie: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Unicode-standaard voor het begrijpen van grafemen: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
