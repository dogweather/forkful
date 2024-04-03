---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:21.420874-07:00
description: "De huidige datum verkrijgen betekent de huidige kalenderdatum van je\
  \ systeem ophalen. Programmeurs doen dit om gebeurtenissen te timestampen, taken\
  \ te\u2026"
lastmod: '2024-03-13T22:44:51.257772-06:00'
model: gpt-4-0125-preview
summary: De huidige datum verkrijgen betekent de huidige kalenderdatum van je systeem
  ophalen.
title: Het huidige datum ophalen
weight: 29
---

## Wat & Waarom?

De huidige datum verkrijgen betekent de huidige kalenderdatum van je systeem ophalen. Programmeurs doen dit om gebeurtenissen te timestampen, taken te plannen, of gewoon om de datum aan gebruikers te tonen.

## Hoe:

In Fish Shell is het een koud kunstje om de huidige datum te pakken te krijgen. Gebruik het `date` commando:

```fish
# Verkrijg de huidige datum in het standaardformaat
date

# Voorbeelduitvoer
Wed Apr  5 15:26:42 PDT 2023

# Verkrijg de huidige datum in een aangepast formaat, bijv. JJJJ-MM-DD
date "+%Y-%m-%d"

# Voorbeelduitvoer
2023-04-05
```

Als je deze wilt toewijzen aan een variabele, doe dan gewoon:

```fish
# Sla de huidige datum op in een variabele
set current_date (date "+%Y-%m-%d")

# Echo de variabele
echo $current_date

# Voorbeelduitvoer
2023-04-05
```

## Diepere Duik

Historisch gezien komt het `date` commando van UNIX, en het bestaat al tientallen jaren. In Fish Shell gebruik je een vriendelijkere versie van dit oude gereedschap. Het `%Y-%m-%d` formaat voor het `date` commando geeft je het jaar, de maand en de dag, maar je hebt een heleboel andere opties zoals `%H` voor uren of `%M` voor minuten.

Waarom Fish gebruiken in plaats van Bash of Zsh hiervoor? Nou, Fish staat bekend om zijn eenvoudigere, leesbaardere syntaxis. Het instellen van variabelen is bijvoorbeeld een stuk duidelijker (`set varname value` vs. `varname=value`), en je hoeft ze niet met `$` te prefixen wanneer je ze gebruikt.

Alternatieven voor de ingebouwde `date` van Fish kunnen zijn het installeren van meer uitgebreide tools zoals `GNU date` voor meer functies, of het benutten van andere Fish-functies of zelfs externe programma's als je meer op maat gemaakt gedrag nodig hebt.

Wat betreft implementatie, wanneer je `date` uitvoert in Fish, gebruik je Fish's wrapper rond het systeem's date commando. Dat betekent dat je op Linux waarschijnlijk `GNU date` gebruikt, terwijl je op macOS de BSD-versie gebruikt. Ze lijken erg op elkaar, maar er zijn enkele subtiele verschillen in de opties die ze ondersteunen.

## Zie Ook

- [Fish Shell Documentatie](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
