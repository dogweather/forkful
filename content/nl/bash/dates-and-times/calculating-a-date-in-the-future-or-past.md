---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:18.793089-07:00
description: 'Hoe te: In Bash kun je het `date` commando gebruiken samen met de `-d`
  vlag om datums te manipuleren. Hier is hoe.'
lastmod: '2024-03-13T22:44:50.997003-06:00'
model: gpt-4-0125-preview
summary: In Bash kun je het `date` commando gebruiken samen met de `-d` vlag om datums
  te manipuleren.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
In Bash kun je het `date` commando gebruiken samen met de `-d` vlag om datums te manipuleren. Hier is hoe:

```Bash
# Huidige Datum
date

# Toekomstige Datum: 10 dagen vanaf nu
date -d "+10days"

# Verleden Datum: 10 dagen geleden
date -d "-10days"

# Specifieke toekomstige datum: Weken, maanden, jaren toevoegen
date -d "+1month"
date -d "+2weeks"
date -d "+1year"

# Voorbeelduitvoer voor toekomstige datum
Ma 31 Jan 2023 12:34:56 PM PST
```

## Diep Duiken
Het manipuleren van datums is een veelvoorkomende behoefte in scripting en programmering. Historisch gezien was deze taak omslachtiger en foutgevoeliger bij het afhandelen van schrikkeljaren, tijdzones, enz. In Unix-achtige systemen is het `date` commando geëvolueerd om opties te omvatten voor eenvoudige datumcalculatie.

Alternatieven zijn het gebruik van shell-aritmetiek of externe hulpmiddelen zoals `awk` of `perl` voor complexere datumlogica, maar het `date` commando blijft de gemakkelijkste en meest rechtlijnige methode voor basisbewerkingen. Onder de motorkap gebruikt het `date` commando systeembibliotheken om de complexiteit van de tijdcalculatie te hanteren, dit wordt geabstraheerd van de gebruiker.

## Zie Ook
- GNU Coreutils Handleiding over Datum: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Meer voorbeelden en gebruiksscenario's: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Gevorderde Bash-Scripting Gids: https://tldp.org/LDP/abs/html/abs-guide.html
