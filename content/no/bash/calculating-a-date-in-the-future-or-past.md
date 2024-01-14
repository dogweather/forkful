---
title:                "Bash: Beregning av dato i fremtiden eller fortiden"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være flere grunner til å ønske å beregne en dato i fremtiden eller fortiden, enten det er for å planlegge en reise, huske på viktige hendelser eller bare for å tilpasse kalenderen din. Heldigvis kan dette enkelt gjøres med Bash-programmering.

## Slik gjør du det
For å beregne en dato i fremtiden eller fortiden i Bash, trenger vi å bruke kommandoen `date` og dens mange muligheter. Her er et eksempel på hvordan du kan beregne datoen 30 dager fra nå:
```Bash
date -d "+30 days"
```
Dette vil gi oss outputen `Mon Nov 15 00:00:00 CET 2021`, noe som betyr 30 dager fra datoen programmet ble kjørt.

For å beregne en dato i fortiden, kan vi bruke kommandoen `date` sammen med parameteren `-` etterfulgt av antall dager. Her er et eksempel på hvordan du kan beregne datoen 100 dager tilbake i tid:
```Bash
date -d "-100 days"
```
Dette vil gi oss outputen `Thu Aug 05 00:00:00 CEST 2021`.

## Dykk dypere
Hvis du vil gå enda dypere i å beregne datoer i Bash, kan du også bruke kommandoen `date` til å arbeide med konkrete datoer. For eksempel, hvis du vil beregne datoen 1. mai neste år, kan du bruke følgende kommando:
```Bash
date -d "1st may next year"
```
Dette vil gi oss outputen `Sun May 01 00:00:00 CEST 2022`.

Det er også mulig å bruke `date` til å arbeide med tidsperioder og endre formatet på datoen som vises. Du kan lese mer om dette og andre nyttige tips i `date`'s manual.

## Se også
- `date`'s manual: https://linux.die.net/man/1/date
- How to format dates in Bash: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- GNU's informative page about the `date` command: https://www.gnu.org/software/coreutils/date