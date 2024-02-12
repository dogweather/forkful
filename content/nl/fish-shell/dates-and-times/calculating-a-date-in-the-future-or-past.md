---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases:
- nl/fish-shell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:28.935075-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van een toekomstige of verleden datum houdt in dat je met datums speelt om erachter te komen welke dag het was of zal zijn. Programmeurs doen dit voor het plannen, herinneringen, of het bepalen van duur en deadlines.

## Hoe te:
Hier is een coole manier om met datums om te gaan in Fish Shell:

```Fish Shell
# Voeg dagen toe aan de huidige datum
set -l days_to_add 10
date -d "+$days_to_add days"

# Voorbeeld van uitvoer (varieert per huidige datum):
# Wo Mar 29 00:29:10 PDT 2023

# Trek dagen af van de huidige datum
set -l days_to_subtract 10
date -d "-$days_to_subtract days"

# Voorbeeld van uitvoer (ook hier kan uw datum variëren):
# Zo Mar 9 00:30:42 PDT 2023
```

## Diepere Duik
Fish gaat niet alleen over de spetters; het heeft ook een geschiedenis. Shells zoals Bash waren vroeger de standaard voor datumcalculaties, meestal via GNU `date`. Fish, die het gestroomlijnd houdt, gebruikt vergelijkbare syntax maar kan gebruiksvriendelijker en leesbaarder zijn – geweldig voor zowel zwemnieuwkomers als ervaren forellen.

Alternatieven voor datumcalculaties omvatten programmeertalen zoals Python of het gebruik van `dateutils`. Elk heeft zijn eigen sterktes, hoewel `dateutils` een beetje obscuur kan zijn en Python misschien te veel van het goede voor eenvoudige taken. Implementatie in Fish is een middenweg, met het `date` commando dat leent van UNIX-standaarden – het is vrijwel overal geïnstalleerd en integreert soepel met systeemtijdinstellingen.

## Zie Ook
Voor meer details, duik in deze waters:
- [GNU Coreutils – Datum](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): Krijg een begrip van hoe `date` onder de motorkap werkt.
- [De Fish Shell Documentatie](https://fishshell.com/docs/current/index.html): Officiële documentatie, waar je kunt leren over Fish en zijn andere commando's.
- [StackOverflow: Datum Rekenkunde](https://stackoverflow.com/questions/tagged/date-arithmetic): Zie echte problemen en oplossingen van de gemeenschap.
