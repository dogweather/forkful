---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases:
- /nl/bash/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:18.793089-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van een datum in de toekomst of het verleden gaat over het vinden van de datum vóór of na een bepaalde periode. Programmeurs doen dit voor taken zoals het instellen van herinneringen, het uitvoeren van geplande taken, of het omgaan met vervaldatums.

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
