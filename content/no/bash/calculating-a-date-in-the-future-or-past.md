---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Bash: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden innebærer å finne den eksakte datoen som er et visst antall dager før eller etter en spesifikk dato. Programmerere gjør dette for planlegging, tidsstyring eller til og med feilsøking. 

## Slik gjør du:

I Bash kan du beregne fremtidige og tidligere datoer ved hjelp av 'date' kommandoen. Her er noen eksempler.

For å beregne en dato 7 dager fra nå:

```Bash
date -d "+7 days"
```

For å beregne en dato 7 dager før nå:

```Bash
date -d "-7 days"
```

Eksempel på utskrift:

```Bash
Mon Jan 12 18:02:37 CET 2022
```

## Dypdykk:

Historisk har dato-beregninger vært utfordrende på grunn av ulike kalendersystemer og tidssoner. I Unix-verdenen, som Bash stammer fra, er dette løst gjennom 'date'-kommandoen, som bruker Unix-tid (sekunder siden 1970).

Alternativt, for mer komplekse dato-beregninger, kan du bruke noe som Python eller Perl, som har rikere datohåndteringsfunksjoner.

Bash bruker 'strtotime'-funksjonen fra biblioteket 'libc' til å parse strengargumentet til 'date'-kommandoen. Derfor er det direkte støtte for relativ dato-beregning som vist i eksemplene over.

## Se også:

- Bash Manual: https://www.gnu.org/software/bash/manual/bash.html
- GNU coreutils (date): https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html